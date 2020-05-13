{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dataflow.Vertices (
  statefulVertex,
  statelessVertex,
  outputTVar
) where

import           Control.Concurrent.STM.TVar (TVar, modifyTVar')
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Class   (lift)
import           Data.Typeable               (Typeable)
import           Dataflow.Primitives         (Dataflow (..), Edge, StateRef,
                                              Timestamp (..), Vertex (..),
                                              newState, registerFinalizer,
                                              registerVertex)
import           Prelude


-- | Construct a vertex with internal state. Like 'statelessVertex', 'statefulVertex'
-- requires a procedure to invoke on each input. It also needs an initial 'state' value
-- and a procedure to call when all inputs for a given 'Timestamp' value have been
-- delivered.
--
-- NB: Until the finalizer has been called for a particular timestamp, a stateful vertex
-- must be capable of accepting data for multiple timestamps simultaneously.
statefulVertex :: Typeable i =>
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
statelessVertex :: Typeable i => (Timestamp -> i -> Dataflow ()) -> Dataflow (Edge i)
statelessVertex callback = registerVertex $ StatelessVertex callback

{-# NOINLINE outputTVar #-}
-- | Construct an output vertex that stores items into the provided 'TVar'. The first argument
-- is an update function so that, for example, the 'TVar' could contain a list of 'o's and the update
-- function could then `cons` new items onto the list.
outputTVar :: Typeable o => (o -> w -> w) -> TVar w -> Dataflow (Edge o)
outputTVar op register = statelessVertex $ \_ x -> Dataflow $ lift $ atomically $ modifyTVar' register (op x)
