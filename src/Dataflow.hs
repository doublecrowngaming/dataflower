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
  execute
) where

import           Control.Monad              (void)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (execStateT, runStateT)
import           Data.Traversable           (Traversable)
import           Dataflow.Primitives
import           Dataflow.Vertices

-- | A 'Program' represents a fully-preprocessed 'Dataflow' that may be
-- executed against inputs.
--
-- @since 0.1.0.0
data Program i = Program {
  programInput :: Edge i,
  programState :: DataflowState
}

-- | Take a 'Dataflow' which takes 'i's as input and compile it into a 'Program'.
--
-- @since 0.1.0.0
compile :: MonadIO io => Dataflow (Edge i) -> io (Program i)
compile (Dataflow actions) = liftIO $ uncurry Program <$> runStateT actions initDataflowState

-- | Feed a traversable collection of inputs to a 'Program'. All inputs provided will
-- have the same 'Timestamp' associated with them.
--
-- @since 0.1.0.0
execute :: (MonadIO io, Traversable t) => t i -> Program i -> io ()
execute corpus Program{..} = liftIO $ execDataflow feedInput
  where
    feedInput           = input corpus programInput
    execDataflow action = void $ execStateT (runDataflow action) programState
