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
  Program,
  compile,
  execute
) where

import           Control.Monad              (void)
import           Control.Monad.State.Strict (execStateT, runStateT)
import           Data.Traversable           (Traversable)
import           Data.Typeable              (Typeable)
import           Dataflow.Primitives
import           Dataflow.Vertices

-- | A 'Program' represents a fully-preprocessed 'Dataflow' that may be
-- executed against inputs.
data Program i = Program {
  programInput :: Edge i,
  programState :: DataflowState
}

-- | Take a 'Dataflow' which takes 'i's as input and compile it into a 'Program'.
compile :: Dataflow (Edge i) -> IO (Program i)
compile (Dataflow actions) = uncurry Program <$> runStateT actions initDataflowState

-- | Feed a traversable collection of inputs to a 'Program'. All inputs provided will
-- have the same 'Timestamp' associated with them.
execute :: (Traversable t, Typeable i) => t i -> Program i -> IO ()
execute corpus Program{..} = execDataflow feedInput
  where
    feedInput           = input corpus programInput
    execDataflow action = void $ execStateT (runDataflow action) programState
