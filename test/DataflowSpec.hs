{-# LANGUAGE ScopedTypeVariables #-}

module DataflowSpec (spec) where

import           Control.Monad   ((>=>))
import           Data.Typeable   (Typeable)
import           Dataflow
import           Prelude         hiding (map)

import           Test.Dataflow   (runDataflow)
import           Test.Hspec
import           Test.QuickCheck


spec :: Spec
spec = do
  it "can pass through data without modification" $ property $ do
    let passthrough next = statelessVertex $ \t x -> send next t x

    \(numbers :: [Integer]) -> runDataflow passthrough numbers `shouldReturn` numbers

  describe "finalize" $ do
    it "finalizes vertices" $ property $
      \(numbers :: [Int]) -> runDataflow storeAndForward numbers `shouldReturn` numbers

    it "finalizes vertices in the correct order" $ property $
      \(numbers :: [Int]) ->
        runDataflow (storeAndForward >=> storeAndForward >=> storeAndForward) numbers `shouldReturn` numbers

storeAndForward :: (Show i, Typeable i) => Edge i -> Dataflow (Edge i)
storeAndForward next = statefulVertex [] store forward
  where
    store sref _ i = modifyState sref (i :)
    forward sref t = do
      mapM_ (send next t) =<< reverse <$> readState sref
      writeState sref []
