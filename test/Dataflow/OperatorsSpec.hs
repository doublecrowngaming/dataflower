{-# LANGUAGE ScopedTypeVariables #-}

module Dataflow.OperatorsSpec (spec) where

import           Dataflow.Operators
import           Prelude            hiding (map)

import           Test.Dataflow      (runDataflow)
import           Test.Hspec
import           Test.QuickCheck    hiding (discard)


spec :: Spec
spec =
  describe "fanout" $
    it "sends all input to each output" $ property $ do
      let fanout' next = fanout [next, next, next]

      \(numbers :: [Int]) -> do
        actual <- runDataflow fanout' numbers
        actual `shouldMatchList` (numbers <> numbers <> numbers)
