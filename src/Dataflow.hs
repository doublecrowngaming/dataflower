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
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (execStateT, runStateT)
import           Dataflow.Primitives
import           Dataflow.Vertices


data Program i = Program {
  programInput :: Edge i,
  programState :: DataflowState
}

compile :: MonadIO io => Dataflow (Edge i) -> io (Program i)
compile (Dataflow actions) = liftIO $ uncurry Program <$> runStateT actions initDataflowState

execute :: MonadIO io => (Edge i -> Dataflow ()) -> Program i -> io ()
execute inputVertex Program{..} = liftIO $ execDataflow feedInput
  where
    feedInput           = inputVertex programInput
    execDataflow action = void $ execStateT (runDataflow action) programState
