{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}

module Dataflow.Primitives (
  Dataflow(..),
  DataflowState,
  Vertex(..),
  initDataflowState,
  duplicateDataflowState,
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
import           Control.Monad              (forM, (>=>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT, get, gets, modify)
import           Control.Monad.Trans        (lift)
import           Data.Hashable              (Hashable (..))
import           Data.IORef                 (IORef, modifyIORef', newIORef,
                                             readIORef, writeIORef)
import           Data.Vector                (Vector, empty, snoc, unsafeIndex)
import           GHC.Exts                   (Any)
import           Numeric.Natural            (Natural)
import           Prelude
import           Unsafe.Coerce              (unsafeCoerce)


newtype VertexID    = VertexID        Int deriving (Eq, Ord, Show)
newtype StateID     = StateID         Int deriving (Eq, Ord, Show)
newtype Epoch       = Epoch       Natural deriving (Eq, Ord, Hashable, Show)

-- | 'Timestamp's represent instants in the causal timeline.
--
-- @since 0.1.0.0
newtype Timestamp   = Timestamp     Epoch deriving (Eq, Ord, Hashable, Show)

-- | An 'Edge' is a typed reference to a computational vertex that
-- takes 'a's as its input.
--
-- @since 0.1.0.0
newtype Edge a      = Edge       VertexID

-- | Class of entities that can be incremented by one.
class Incrementable a where
  inc :: a -> a

instance Incrementable VertexID where
  inc (VertexID n) = VertexID (n + 1)

instance Incrementable StateID where
  inc (StateID n) = StateID (n + 1)

instance Incrementable Epoch where
  inc (Epoch n) = Epoch (n + 1)


data DataflowState = DataflowState {
  dfsVertices       :: Vector Any,
  dfsStates         :: Vector (IORef Any),
  dfsFinalizers     :: [Timestamp -> Dataflow ()],
  dfsLastVertexID   :: VertexID,
  dfsLastStateID    :: StateID,
  dfsLastInputEpoch :: Epoch
}

-- | `Dataflow` is the type of all dataflow operations.
--
-- @since 0.1.0.0
newtype Dataflow a = Dataflow { runDataflow :: StateT DataflowState IO a }
  deriving (Functor, Applicative, Monad)

initDataflowState :: DataflowState
initDataflowState = DataflowState {
  dfsVertices       = empty,
  dfsStates         = empty,
  dfsFinalizers     = [],
  dfsLastVertexID   = VertexID (-1),
  dfsLastStateID    = StateID (-1),
  dfsLastInputEpoch = Epoch 0
}

duplicateDataflowState :: Dataflow DataflowState
duplicateDataflowState = Dataflow $ do
  DataflowState{..} <- get

  newStates <- liftIO $ forM dfsStates dupIORef

  return $ DataflowState { dfsStates = newStates, .. }

  where
    dupIORef = readIORef >=> newIORef

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
lookupVertex :: Edge i -> Dataflow (Vertex i)
lookupVertex (Edge (VertexID vindex)) =
  Dataflow $ do
    vertices <- gets dfsVertices

    return $ unsafeCoerce (vertices `unsafeIndex` vindex)

-- | Store a provided vertex and obtain an 'Edge' that refers to it.
registerVertex :: Vertex i -> Dataflow (Edge i)
registerVertex vertex =
  Dataflow $ do
    vid <- gets (dfsLastVertexID >>> inc)

    modify $ addVertex vertex vid

    return (Edge vid)

  where
    addVertex vtx vid s = s {
      dfsVertices     = dfsVertices s `snoc` unsafeCoerce vtx,
      dfsLastVertexID = vid
    }

-- | Store a provided finalizer.
registerFinalizer :: (Timestamp -> Dataflow ()) -> Dataflow ()
registerFinalizer finalizer =
  Dataflow $ modify $ \s -> s { dfsFinalizers = finalizer : dfsFinalizers s }

-- | Mutable state that holds an `a`.
--
-- @since 0.1.0.0
newtype StateRef a = StateRef StateID

-- | Create a `StateRef` initialized to the provided `a`.
--
-- @since 0.1.0.0
newState :: a -> Dataflow (StateRef a)
newState a =
  Dataflow $ do
    sid   <- gets (dfsLastStateID >>> inc)
    ioref <- lift $ newIORef (unsafeCoerce a)

    modify $ addState ioref sid

    return (StateRef sid)

  where
    addState ref sid s = s {
      dfsStates      = dfsStates s `snoc` ref,
      dfsLastStateID = sid
    }

lookupStateRef :: StateRef s -> Dataflow (IORef Any)
lookupStateRef (StateRef (StateID sindex)) =
  Dataflow $ do
    states <- gets dfsStates

    return (states `unsafeIndex` sindex)

-- | Read the value stored in the `StateRef`.
--
-- @since 0.1.0.0
readState :: StateRef a -> Dataflow a
readState sref = do
  ioref <- lookupStateRef sref
  Dataflow $ lift (unsafeCoerce <$> readIORef ioref)

-- | Overwrite the value stored in the `StateRef`.
--
-- @since 0.1.0.0
writeState :: StateRef a -> a -> Dataflow ()
writeState sref x = do
  ioref <- lookupStateRef sref
  Dataflow $ lift $ writeIORef ioref (unsafeCoerce x)

-- | Update the value stored in `StateRef`.
--
-- @since 0.1.0.0
modifyState :: StateRef a -> (a -> a) -> Dataflow ()
modifyState sref op = do
  ioref <- lookupStateRef sref
  Dataflow $ lift $ modifyIORef' ioref (unsafeCoerce . op . unsafeCoerce)

{-# INLINEABLE input #-}
input :: Traversable t => t i -> Edge i -> Dataflow ()
input inputs next = do
  timestamp <- Timestamp <$> incrementEpoch

  mapM_ (send next timestamp) inputs

  finalize timestamp

{-# INLINE send #-}
-- | Send an `input` item to be worked on to the indicated vertex.
--
-- @since 0.1.0.0
send :: Edge input -> Timestamp -> input -> Dataflow ()
send e t i = lookupVertex e >>= invoke t i
  where
    invoke timestamp datum (StatefulVertex sref callback) = callback sref timestamp datum
    invoke timestamp datum (StatelessVertex callback)     = callback timestamp datum

-- Notify all relevant vertices that no more input is coming for `Timestamp`.
--
-- @since 0.1.0.0
finalize :: Timestamp -> Dataflow ()
finalize t = do
  finalizers <- Dataflow $ gets dfsFinalizers

  mapM_ (\p -> p t) finalizers
