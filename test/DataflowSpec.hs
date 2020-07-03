{-# LANGUAGE ScopedTypeVariables #-}

module DataflowSpec (spec) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar,
                                              readTVarIO, writeTVar)
import           Control.Monad               (void, (>=>))
import           Dataflow
import           Prelude

import           Data.ByteString             (ByteString)
import           Data.Store                  (Store)
import           Data.Vector                 (Vector, empty)
import           Test.Dataflow               (runDataflow)
import           Test.Hspec
import           Test.QuickCheck             hiding (discard)
import           Test.QuickCheck.Modifiers   (NonEmptyList (..))


memoryPersistor :: TVar (Vector ByteString) -> Persistor
memoryPersistor tvar =
  Persistor
    (atomically $ readTVar tvar)
    (atomically . writeTVar tvar)

spec :: Spec
spec = do
  it "can pass through data without modification" $ property $ do
    let passthrough next = statelessVertex $ \t x -> send next t x

    \(numbers :: [Integer]) -> runDataflow passthrough numbers `shouldReturn` numbers

  describe "execute" $ do
    it "isolates the state of runs from each other" $ property $ \(NonEmpty numbers) -> do
      out     <- newTVarIO []
      program <- compile (integrate =<< outputTVar (:) out)

      void $ execute numbers program
      void $ execute numbers program

      (reverse <$> readTVarIO out) `shouldReturn` (scanl1 (+) numbers ++ scanl1 (+) numbers)

    it "bundles all the execution state into a Program" $ property $ \(NonEmpty numbers) -> do
      out     <- newTVarIO 0
      program <- compile (integrate =<< outputTVar const out)

      void $ execute numbers program >>= execute numbers >>= execute numbers

      readTVarIO out `shouldReturn` (3 * sum numbers)

  describe "State persistence" $
    it "works" $ property $ \(NonEmpty numbers) -> do
      persist  <- memoryPersistor <$> newTVarIO empty
      out1     <- newTVarIO 0
      out2     <- newTVarIO 0
      program1 <- compileNoLoad (integrate =<< outputTVar const out1) persist

      void $ execute numbers program1 >>= execute numbers >>= execute numbers

      program2 <- compileAndLoad (integrate =<< outputTVar const out2) persist

      void $ execute numbers program2

      readTVarIO out1 `shouldReturn` (3 * sum numbers)
      readTVarIO out2 `shouldReturn` (4 * sum numbers)

  describe "finalize" $ do
    it "finalizes vertices" $ property $
      \(numbers :: [Int]) -> runDataflow storeAndForward numbers `shouldReturn` numbers

    it "finalizes vertices in the correct order" $ property $
      \(numbers :: [Int]) ->
        runDataflow (storeAndForward >=> storeAndForward >=> storeAndForward) numbers `shouldReturn` numbers

  describe "discard" $
    it "discards all input" $ property $
      \(numbers :: [Int]) -> runDataflow (const discard) numbers `shouldReturn` ([] :: [Int])

storeAndForward :: (Store i, Show i) => Edge i -> Dataflow (Edge i)
storeAndForward next = statefulVertex [] store forward
  where
    store sref _ i = modifyState sref (i :)
    forward sref t = do
      mapM_ (send next t) =<< reverse <$> readState sref
      writeState sref []

integrate :: Edge Int -> Dataflow (Edge Int)
integrate next = statefulVertex 0 recv finalize
  where
    recv s t i   = do
      modifyState s (+ i)

      send next t =<< readState s

    finalize _ _ = return ()
