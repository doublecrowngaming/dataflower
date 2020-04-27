{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Dataflow.Primitives (
  Dataflow(..),
  DataflowState,
  Vertex(..),
  initDataflowState,
  StateRef,
  newState,
  readState,
  writeState,
  modifyState,
  Edge,
  Timestamp(..),
  registerVertex,
  registerFinalizer,
  incrementEpoch,
  input,
  send,
  finalize
) where

import           Control.Arrow              ((>>>))
import           Control.Monad.State.Strict (StateT, gets, modify)
import           Control.Monad.Trans        (lift)
import           Data.Dynamic               (Dynamic, Typeable, fromDyn, toDyn)
import           Data.Hashable              (Hashable (..))
import           Data.IORef                 (IORef, atomicModifyIORef',
                                             atomicWriteIORef, newIORef,
                                             readIORef)
import           Data.Vector                (Vector, empty, snoc, (!))
import           Numeric.Natural            (Natural)
import           Prelude


newtype VertexID    = VertexID        Int deriving (Eq, Ord, Show)
newtype Epoch       = Epoch       Natural deriving (Eq, Ord, Hashable, Show)

-- | 'Timestamp's represent instants in the causal timeline.
newtype Timestamp   = Timestamp     Epoch deriving (Eq, Ord, Hashable, Show)

-- | An 'Edge' is a typed reference to a computational vertex that
-- takes 'a's as its input.
newtype Edge a      = Edge       VertexID

-- | Class of entities that can be incremented by one.
class Incrementable a where
  inc :: a -> a

instance Incrementable VertexID where
  inc (VertexID n) = VertexID (n + 1)

instance Incrementable Epoch where
  inc (Epoch n) = Epoch (n + 1)


data DataflowState = DataflowState {
  dfsVertices       :: Vector Dynamic,
  dfsFinalizers     :: [Timestamp -> Dataflow ()],
  dfsLastVertexID   :: VertexID,
  dfsLastInputEpoch :: Epoch
}

-- | `Dataflow` is the type of all dataflow operations.
newtype Dataflow a = Dataflow { runDataflow :: StateT DataflowState IO a }
  deriving (Functor, Applicative, Monad)

initDataflowState :: DataflowState
initDataflowState = DataflowState {
  dfsVertices       = empty,
  dfsFinalizers     = [],
  dfsLastVertexID   = VertexID (-1),
  dfsLastInputEpoch = Epoch 0
}

-- | Get the next input Epoch.
incrementEpoch :: Dataflow Epoch
incrementEpoch =
  Dataflow $ do
    epoch <- gets (dfsLastInputEpoch >>> inc)

    modify $ \s -> s { dfsLastInputEpoch = epoch }

    return epoch


data Vertex i = forall s.
    StatefulVertex
      (StateRef s)
      (StateRef s -> Timestamp -> i -> Dataflow ())
  | StatelessVertex
      (Timestamp -> i -> Dataflow ())

-- | Retrieve the vertex for a given edge.
lookupVertex :: Typeable i => Edge i -> Dataflow (Vertex i)
lookupVertex (Edge (VertexID vindex)) =
  Dataflow $ do
    vertices <- gets dfsVertices

    return $ fromDyn (vertices ! vindex) (error "Programming error: Vertex and Edge were of different types")

-- | Store a provided vertex and obtain an 'Edge' that refers to it.
registerVertex :: Typeable i => Vertex i -> Dataflow (Edge i)
registerVertex vertex =
  Dataflow $ do
    vid <- gets (dfsLastVertexID >>> inc)

    modify $ addVertex vertex vid

    return (Edge vid)

  where
    addVertex vtx vid s = s {
      dfsVertices     = dfsVertices s `snoc` toDyn vtx,
      dfsLastVertexID = vid
    }

-- | Store a provided finalizer.
registerFinalizer :: (Timestamp -> Dataflow ()) -> Dataflow ()
registerFinalizer finalizer =
  Dataflow $ modify $ \s -> s { dfsFinalizers = finalizer : dfsFinalizers s }

-- | Mutable state that holds an `a`.
newtype StateRef a = StateRef (IORef a)

-- | Create a `StateRef` initialized to the provided `a`.
newState :: a -> Dataflow (StateRef a)
newState a = StateRef <$> (Dataflow $ lift $ newIORef a)

-- | Read the value stored in the `StateRef`.
readState :: StateRef a -> Dataflow a
readState (StateRef ref) = Dataflow $ lift (readIORef ref)

-- | Overwrite the value stored in the `StateRef`.
writeState :: StateRef a -> a -> Dataflow ()
writeState (StateRef ref) x = Dataflow $ lift $ atomicWriteIORef ref x

-- | Update the value stored in `StateRef`.
modifyState :: StateRef a -> (a -> a) -> Dataflow ()
modifyState (StateRef ref) op = Dataflow $ lift $ atomicModifyIORef' ref (\x -> (op x, ()))

{-# INLINEABLE input #-}
input :: (Traversable t, Typeable i) => t i -> Edge i -> Dataflow ()
input inputs next = do
  timestamp <- Timestamp <$> incrementEpoch

  mapM_ (send next timestamp) inputs

  finalize timestamp

{-# NOINLINE send #-}
-- | Send an `input` item to be worked on to the indicated vertex.
send :: Typeable input => Edge input -> Timestamp -> input -> Dataflow ()
send e t i = lookupVertex e >>= invoke t i
  where
    invoke timestamp datum (StatefulVertex sref callback) = callback sref timestamp datum
    invoke timestamp datum (StatelessVertex callback)     = callback timestamp datum

-- Notify all relevant vertices that no more input is coming for `Timestamp`.
finalize :: Timestamp -> Dataflow ()
finalize t = do
  finalizers <- Dataflow $ gets dfsFinalizers

  mapM_ (\p -> p t) finalizers
