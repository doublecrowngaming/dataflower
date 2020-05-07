module Main where

import           Criterion.Main
import           Data.Typeable  (Typeable)
import           Dataflow
import           Prelude
import           Test.Dataflow  (runDataflow)


main :: IO ()
main = defaultMain [
  bgroup "dataflow" [
      bench "1,000,000 inputs" $ nfIO (runDataflow blackhole [0..1000000 :: Int])
    ]
  ]
  where
    blackhole :: Typeable a => Edge a -> Dataflow (Edge a)
    blackhole _ = discard
