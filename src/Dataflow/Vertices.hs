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
import           Dataflow.Primitives         (Dataflow (..), Edge, StateRef,
                                              Timestamp (..), Vertex (..),
                                              finalize, incrementEpoch,
                                              newState, registerFinalizer,
                                              registerVertex, send)
import           Prelude
import           Text.Show.Pretty            (pPrint)


statefulVertex ::
  state
  -> (StateRef state -> Timestamp -> i -> Dataflow ())
  -> (StateRef state -> Timestamp -> Dataflow ())
  -> Dataflow (Edge i)
statefulVertex initState callback finalizer = do
  stateRef <- newState initState

  registerFinalizer $ finalizer stateRef
  registerVertex    $ StatefulVertex stateRef callback

statelessVertex :: (Timestamp -> i -> Dataflow ()) -> Dataflow (Edge i)
statelessVertex callback = registerVertex $ StatelessVertex callback

ioVertex :: (Timestamp -> i -> IO ()) -> Dataflow (Edge i)
ioVertex ioCallback = registerVertex $ StatelessVertex callback
  where
    callback t i = Dataflow $ lift $ ioCallback t i

{-# INLINEABLE input #-}
input :: Traversable t => t i -> Edge i -> Dataflow ()
input inputs next = do
  timestamp <- Timestamp <$> incrementEpoch

  mapM_ (send next timestamp) inputs

  finalize timestamp

{-# NOINLINE output #-}
output :: (o -> w -> w) -> TVar w -> Dataflow (Edge o)
output op register = statelessVertex $ \_ x -> Dataflow $ lift $ atomically $ modifyTVar' register (op x)


trace :: Show i => Edge i -> Dataflow (Edge i)
trace next = do
  trace' <- ioVertex $ curry pPrint

  statelessVertex $ \t x -> do
    send trace' t x
    send next t x

discard :: Dataflow (Edge i)
discard = statelessVertex $ \_ _ -> return ()
