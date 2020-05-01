{-# LANGUAGE ScopedTypeVariables #-}

module Dataflow.OperatorsSpec (spec) where

import           Dataflow.Operators
import           Prelude            hiding (map)
import qualified Prelude            (map)

import           Test.Dataflow      (runDataflow)
import           Test.Hspec
import           Test.QuickCheck    hiding (discard)


spec :: Spec
spec = do
  describe "fanout" $
    it "sends all input to each output" $ property $ do
      let fanout' next = fanout [next, next, next]

      \(numbers :: [Int]) -> do
        actual <- runDataflow fanout' numbers
        actual `shouldMatchList` (numbers <> numbers <> numbers)

  describe "map" $
    it "applies a function to all values passing through" $ property $
      \(numbers :: [Int]) -> runDataflow (map (+ 1)) numbers `shouldReturn` Prelude.map (+ 1) numbers
