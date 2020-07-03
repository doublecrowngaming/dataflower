{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Dataflow
Description : Timely Dataflow for Haskell
Copyright   : (c) Double Crown Gaming Co. 2020
License     : BSD3
Maintainer  : jesse.kempf@doublecrown.co
Stability   : experimental

Timely Dataflow in pure Haskell.
-}

module Dataflow (
  Dataflow,
  Edge,
  Timestamp,
  StateRef,
  send,
  readState,
  writeState,
  modifyState,
  statefulVertex,
  statelessVertex,
  outputTVar,
  trace,
  discard,
  Program,
  compile,
  compileAndLoad,
  compileNoLoad,
  execute,
  Persistor(..)
) where

import           Control.Monad              (void)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (execStateT, runStateT)
import           Data.ByteString            (ByteString)
import           Data.Traversable           (Traversable)
import           Data.Vector                (Vector)
import           Dataflow.Primitives
import           Dataflow.Vertices

-- | A 'Program' represents a fully-preprocessed 'Dataflow' that may be
-- executed against inputs.
--
-- @since 0.1.0.0
data Program i = Program {
  programInput     :: Edge i,
  programState     :: DataflowState,
  programPersistor :: Maybe Persistor
}

-- | Take a 'Dataflow' which takes 'i's as input and compile it into a 'Program'.
--
-- @since 0.3.0.0
data Persistor = Persistor {
  persistorLoad  :: IO (Vector ByteString),
  persistorStore :: Vector ByteString -> IO ()
}

-- | Take a 'Dataflow' which takes 'i's as input and compile it into a 'Program'.
-- Use the provided 'Persistor' to persist state after each timestamp has been
-- processed fully.
--
-- This procedure loads persisted state automatically.
--
-- @since 0.3.0.0
compileAndLoad :: MonadIO io => Dataflow (Edge i) -> Persistor -> io (Program i)
compileAndLoad dataflow persistor = do
  Program{..} <- compile dataflow

  state <- liftIO (decodeStates programState =<< persistorLoad persistor)

  return Program {
    programState     = state,
    programPersistor = Just persistor,
    ..
  }

-- | Take a 'Dataflow' which takes 'i's as input and compile it into a 'Program'.
-- Use the provided 'Persistor' to persist state after each timestamp has been
-- processed fully.
--
-- This procedure will not load persisted state. Use this as a fallback for when
-- there is trouble decoding persistent state, or if the state does not exist.
--
-- @since 0.3.0.0
compileNoLoad :: MonadIO io => Dataflow (Edge i) -> Persistor -> io (Program i)
compileNoLoad dataflow persistor = do
  Program{..} <- compile dataflow

  return Program {
    programPersistor = Just persistor,
    ..
  }

-- | Take a 'Dataflow' which takes 'i's as input and compile it into a 'Program'.
--
-- @since 0.1.0.0
compile :: MonadIO io => Dataflow (Edge i) -> io (Program i)
compile (Dataflow actions) = liftIO $ do
  (inputEdge, initState) <- runStateT actions initDataflowState

  return (Program inputEdge initState Nothing)


-- | Feed a traversable collection of inputs to a 'Program'. All inputs provided will
-- have the same 'Timestamp' associated with them.
--
-- @since 0.1.0.0
execute :: (MonadIO io, Traversable t) => t i -> Program i -> io (Program i)
execute corpus Program{..} = liftIO $ do
  newProgramState <- duplicateDataflowState programState

  void $ execStateT (runDataflow $ input corpus programInput) newProgramState

  case programPersistor of
    Nothing  -> return ()
    (Just p) -> encodeStates newProgramState >>= persistorStore p

  return $ Program programInput newProgramState programPersistor
