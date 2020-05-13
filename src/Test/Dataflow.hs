module Test.Dataflow (
  runDataflow
) where

import           Control.Concurrent.STM.TVar (newTVarIO, readTVarIO)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Dataflow                    (Dataflow, Edge, compile, execute,
                                              outputTVar)
import           Prelude


runDataflow :: MonadIO io => (Edge o -> Dataflow (Edge i)) -> [i] -> io [o]
runDataflow dataflow inputs =
  liftIO $ do
    out     <- newTVarIO []
    program <- compile (dataflow =<< outputTVar (:) out)

    execute inputs program

    reverse <$> readTVarIO out
