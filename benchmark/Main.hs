module Main where

import           Criterion.Main
import           Dataflow
import           Prelude
import           Test.Dataflow  (runDataflow)


main :: IO ()
main = defaultMain [
  bgroup "dataflow" [
      bench "passthrough 1,000,000 inputs" $ nfIO (runDataflow passthrough [0..1000000 :: Int]),
      bench "discard         1,000 inputs" $ nfIO (runDataflow blackhole [0..1000    :: Int]),
      bench "discard     1,000,000 inputs" $ nfIO (runDataflow blackhole [0..1000000 :: Int])
    ]
  ]
  where
    passthrough :: Edge a -> Dataflow (Edge a)
    passthrough next = statelessVertex $ \t x -> send next t x

    blackhole :: Edge a -> Dataflow (Edge a)
    blackhole _ = statelessVertex $ \_ _ -> return ()
