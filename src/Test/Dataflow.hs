module Test.Dataflow (
  runDataflow
) where

import           Control.Concurrent.STM.TVar (newTVarIO, readTVarIO)
import           Control.Monad
import           Control.Monad.IO.Class      (MonadIO (..))
import           Data.Typeable               (Typeable)
import           Dataflow
import           Prelude


runDataflow :: (Typeable i, Typeable o, MonadIO io) => (Edge o -> Dataflow (Edge i)) -> [i] -> io [o]
runDataflow dataflow inputs =
  liftIO $ do
    out     <- newTVarIO []
    program <- compile (dataflow =<< output (:) out)

    execute (input inputs) program

    reverse <$> readTVarIO out
