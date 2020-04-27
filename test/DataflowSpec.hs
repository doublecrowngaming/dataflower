{-# LANGUAGE ScopedTypeVariables #-}

module DataflowSpec (spec) where

import           Dataflow
import           Prelude         hiding (map)

import           Test.Dataflow   (runDataflow)
import           Test.Hspec
import           Test.QuickCheck


spec :: Spec
spec =
  it "can pass through data without modification" $ property $ do
    let passthrough next = statelessVertex $ \t x -> send next t x

    \(numbers :: [Integer]) -> runDataflow passthrough numbers `shouldReturn` numbers
