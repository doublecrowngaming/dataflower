module Main where

import           Criterion.Main
import           Data.Typeable  (Typeable)
import           Dataflow
import           Prelude
import           Test.Dataflow  (runDataflow)


main :: IO ()
main = defaultMain [
  bgroup "dataflow" [
      bench "passthrough 1,000,000 inputs" $ nfIO (runDataflow passthrough [0..1000000 :: Int]),
      bench "discard     1,000,000 inputs" $ nfIO (runDataflow blackhole [0..1000000 :: Int])
    ]
  ]
  where
    passthrough :: Typeable a => Edge a -> Dataflow (Edge a)
    passthrough next = statelessVertex $ \t x -> send next t x

    blackhole :: Typeable a => Edge a -> Dataflow (Edge a)
    blackhole _ = statelessVertex $ \_ _ -> return ()
