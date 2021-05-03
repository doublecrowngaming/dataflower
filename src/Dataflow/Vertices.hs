{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dataflow.Vertices (
  statefulVertex,
  statelessVertex,
  outputTVar,
  trace,
  discard
) where

import           Control.Concurrent.STM.TVar (TVar, modifyTVar')
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Class   (lift)
import           Dataflow.Primitives         (Dataflow (..), Edge, StateRef,
                                              Timestamp (..), Vertex (..),
                                              newState, registerFinalizer,
                                              registerVertex, send)
import           Prelude
import           Text.Show.Pretty            (pPrint)


-- | Construct a vertex with internal state. Like 'statelessVertex', 'statefulVertex'
-- requires a procedure to invoke on each input. It also needs an initial 'state' value
-- and a procedure to call when all inputs for a given 'Timestamp' value have been
-- delivered.
--
-- NB: Until the finalizer has been called for a particular timestamp, a stateful vertex
-- must be capable of accepting data for multiple timestamps simultaneously.
--
-- @since 0.1.0.0
statefulVertex ::
  state -- ^ The initial state value.
  -> (StateRef state -> Timestamp -> i -> Dataflow ()) -- ^ The input handler.
  -> (StateRef state -> Timestamp -> Dataflow ()) -- ^ The finalizer.
  -> Dataflow (Edge i)
statefulVertex initState callback finalizer = do
  stateRef <- newState initState

  registerFinalizer $ finalizer stateRef
  registerVertex    $ StatefulVertex stateRef callback

-- | Construct a vertex with no internal state. The given procedure is invoked on each input.
--
-- `send`ing to a stateless vertex is effectively a function call and will execute in the
-- caller's thread. By design this is a cheap operation.
--
-- @since 0.1.0.0
statelessVertex :: (Timestamp -> i -> Dataflow ()) -> Dataflow (Edge i)
statelessVertex callback = registerVertex $ StatelessVertex callback

{-# NOINLINE outputTVar #-}
-- | Construct an output vertex that stores items into the provided 'TVar'. The first argument
-- is an update function so that, for example, the 'TVar' could contain a list of 'o's and the update
-- function could then `cons` new items onto the list.
--
-- @since 0.1.0.0
outputTVar :: (o -> w -> w) -> TVar w -> Dataflow (Edge o)
outputTVar op register = statelessVertex $ \_ x -> Dataflow $ lift $ atomically $ modifyTVar' register (op x)

-- | Construct a vertex that pretty-prints items and passes them through unchanged.
--
-- @since 0.1.2.0
trace :: Show i => Edge i -> Dataflow (Edge i)
trace next = do
  trace' <- ioVertex $ curry pPrint

  statelessVertex $ \t x -> do
    send trace' t x
    send next t x

  where
    ioVertex callback = registerVertex $ StatelessVertex $ \t i -> Dataflow $ lift $ callback t i

-- | Construct a vertex that discards anything sent to it.
--
-- @since 0.1.2.0
discard :: Dataflow (Edge i)
discard = statelessVertex $ \_ _ -> return ()
