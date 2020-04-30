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
  send,
  finalize
) where

import           Control.Arrow              ((>>>))
import           Control.Monad.State.Strict (StateT, gets, modify)
import           Control.Monad.Trans        (lift)
import           Data.Dynamic               (Dynamic, Typeable, fromDynamic,
                                             toDyn)
import           Data.IORef                 (IORef, atomicModifyIORef',
                                             atomicWriteIORef, newIORef,
                                             readIORef)
import           Data.Vector                (Vector, empty, snoc, (!))
import           Numeric.Natural            (Natural)
import           Prelude


newtype VertexID    = VertexID        Int deriving (Eq, Ord, Show)
newtype Epoch       = Epoch       Natural deriving (Eq, Ord, Show)
newtype Timestamp   = Timestamp     Epoch deriving (Eq, Ord, Show)
newtype Edge o      = Edge       VertexID deriving (Eq, Ord, Show)

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

newtype Dataflow a = Dataflow { runDataflow :: StateT DataflowState IO a }
  deriving (Functor, Applicative, Monad)

initDataflowState :: DataflowState
initDataflowState = DataflowState {
  dfsVertices       = empty,
  dfsFinalizers     = [],
  dfsLastVertexID   = VertexID (-1),
  dfsLastInputEpoch = Epoch 0
}

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

lookupVertex :: Typeable i => Edge i -> Dataflow (Vertex i)
lookupVertex (Edge vid) = retrieveVertex vid >>= ensureTypecast
  where
    retrieveVertex (VertexID vindex) = Dataflow (fromDynamic <$> gets (dfsVertices >>> (! vindex)))

    ensureTypecast Nothing  = error "Programming error: Vertex and Edge were of different types"
    ensureTypecast (Just v) = return v

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

registerFinalizer :: (Timestamp -> Dataflow ()) -> Dataflow ()
registerFinalizer finalizer =
  Dataflow $ modify $ \s -> s { dfsFinalizers = finalizer : dfsFinalizers s }

newtype StateRef a = StateRef (IORef a)

newState :: a -> Dataflow (StateRef a)
newState a = StateRef <$> (Dataflow $ lift $ newIORef a)

readState :: StateRef a -> Dataflow a
readState (StateRef ref) = Dataflow $ lift (readIORef ref)

writeState :: StateRef a -> a -> Dataflow ()
writeState (StateRef ref) x = Dataflow $ lift $ atomicWriteIORef ref x

modifyState :: StateRef a -> (a -> a) -> Dataflow ()
modifyState (StateRef ref) op = Dataflow $ lift $ atomicModifyIORef' ref (\x -> (op x, ()))


{-# NOINLINE send #-}
send :: Typeable i => Edge i -> Timestamp -> i -> Dataflow ()
send e t i = lookupVertex e >>= invoke t i
  where
    invoke timestamp datum (StatefulVertex sref callback) = callback sref timestamp datum
    invoke timestamp datum (StatelessVertex callback)     = callback timestamp datum

finalize :: Timestamp -> Dataflow ()
finalize t = do
  finalizers <- Dataflow $ gets dfsFinalizers

  mapM_ (\p -> p t) finalizers
