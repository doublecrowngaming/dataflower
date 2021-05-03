{-# LANGUAGE ScopedTypeVariables #-}

module Dataflow.OperatorsSpec (spec) where

import           Data.Functor.Identity (Identity (..))
import           Data.Semigroup        (Sum (..))
import           Dataflow
import           Dataflow.Operators
import           Prelude               hiding (map)
import qualified Prelude               (map)

import           Test.Dataflow         (runDataflow)
import           Test.Hspec
import           Test.QuickCheck       hiding (discard)


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

  describe "join2" $
    it "accepts two different kinds of input" $ property $ \numbers -> do
      let numJoin next = join2 (0 :: Int)
                          (\sref _ i            -> modifyState sref (+ i))
                          (\sref _ (Identity j) -> modifyState sref (+ j))
                          (\sref t -> send next t =<< readState sref)
          numbers'     = Prelude.map Identity numbers

      runDataflow (fmap fst . numJoin) numbers  `shouldReturn` [sum numbers]
      runDataflow (fmap snd . numJoin) numbers' `shouldReturn` [sum numbers]

  describe "join3" $
    it "accepts three different kinds of input" $ property $ \numbers -> do
      let numJoin next   = join3 (0 :: Int)
                            (\sref _ i            -> modifyState sref (+ i))
                            (\sref _ (Identity j) -> modifyState sref (+ j))
                            (\sref _ (Sum k)      -> modifyState sref (+ k))
                            (\sref t -> send next t =<< readState sref)
          numbers'       = Prelude.map Identity numbers
          numbers''      = Prelude.map Sum numbers
          fst3 (a, _, _) = a
          snd3 (_, b, _) = b
          trd3 (_, _, c) = c

      runDataflow (fmap fst3 . numJoin) numbers   `shouldReturn` [sum numbers]
      runDataflow (fmap snd3 . numJoin) numbers'  `shouldReturn` [sum numbers]
      runDataflow (fmap trd3 . numJoin) numbers'' `shouldReturn` [sum numbers]
