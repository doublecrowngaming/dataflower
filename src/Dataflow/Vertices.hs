{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dataflow.Vertices (
  statefulVertex,
  statelessVertex,
  input,
  output,
  trace,
  discard
) where

import           Control.Concurrent.STM.TVar (TVar, modifyTVar')
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Class   (lift)
import           Data.Traversable            (Traversable)
import           Data.Typeable               (Typeable)
import           Dataflow.Primitives         (Dataflow (..), Edge, StateRef,
                                              Timestamp (..), Vertex (..),
                                              incrementEpoch, newState,
                                              registerVertex, send)
import           Prelude
import           Text.Show.Pretty            (pPrint)


statefulVertex :: Typeable i => s -> (StateRef s -> Timestamp -> i -> Dataflow ()) -> Dataflow (Edge i)
statefulVertex initState callback = do
  stateRef   <- newState initState
  registerVertex $ StatefulVertex stateRef callback

statelessVertex :: Typeable i => (Timestamp -> i -> Dataflow ()) -> Dataflow (Edge i)
statelessVertex callback = registerVertex $ StatelessVertex callback

ioVertex :: Typeable i => (Timestamp -> i -> IO ()) -> Dataflow (Edge i)
ioVertex ioCallback = registerVertex $ StatelessVertex callback
  where
    callback t i = Dataflow $ lift $ ioCallback t i

{-# INLINEABLE input #-}
input :: (Traversable t, Typeable i) => t i -> Edge i -> Dataflow ()
input inputs next = do
  timestamp <- Timestamp <$> incrementEpoch

  mapM_ (send next timestamp) inputs

{-# NOINLINE output #-}
output :: Typeable o => (o -> w -> w) -> TVar w -> Dataflow (Edge o)
output op register = statelessVertex $ \_ x -> Dataflow $ lift $ atomically $ modifyTVar' register (op x)


trace :: (Typeable i, Show i) => Edge i -> Dataflow (Edge i)
trace next = do
  trace' <- ioVertex $ curry pPrint

  statelessVertex $ \t x -> do
    send trace' t x
    send next t x

discard :: Typeable i => Dataflow (Edge i)
discard = statelessVertex $ \_ _ -> return ()
