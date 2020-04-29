{-# LANGUAGE RecordWildCards #-}

module Dataflow (
  Dataflow,
  Edge,
  send,
  readState,
  writeState,
  modifyState,
  statefulVertex,
  statelessVertex,
  input,
  output,
  trace,
  discard,
  Program,
  compile,
  execute
) where

import           Control.Monad              (void)
import           Control.Monad.State.Strict (execStateT, runStateT)
import           Dataflow.Primitives
import           Dataflow.Vertices


data Program i = Program {
  programInput :: Edge i,
  programState :: DataflowState
}

compile :: Dataflow (Edge i) -> IO (Program i)
compile (Dataflow actions) = uncurry Program <$> runStateT actions initDataflowState

execute :: (Edge i -> Dataflow ()) -> Program i -> IO ()
execute inputVertex Program{..} = execDataflow feedInput
  where
    feedInput           = inputVertex programInput
    execDataflow action = void $ execStateT (runDataflow action) programState
