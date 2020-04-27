module Main where

import           Criterion.Main
import           Dataflow
import           Prelude
import           Test.Dataflow  (runDataflow)


main :: IO ()
main = defaultMain [
  bgroup "dataflow" [
      bench "1,000,000 inputs" $ nfIO (runDataflow passthrough [0..1000000 :: Int])
    ]
  ]
  where
    passthrough next = statelessVertex $ \t x -> send next t x
