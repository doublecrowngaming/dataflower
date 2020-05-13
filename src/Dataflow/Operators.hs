{-|
Module      : Dataflow
Description : Timely Dataflow for Haskell
Copyright   : (c) Double Crown Gaming Co. 2020
License     : BSD3
Maintainer  : jesse.kempf@doublecrown.co
Stability   : experimental

Common utility operators for data flows

@since 0.1.3.0
-}

module Dataflow.Operators (
  fanout,
  map,
  join2,
  join3
) where

import           Dataflow.Primitives (Dataflow, Edge, StateRef, Timestamp,
                                      Vertex (StatefulVertex), newState,
                                      registerFinalizer, registerVertex, send)
import           Dataflow.Vertices   (statelessVertex)
import           Prelude             (mapM_, ($), (<$>), (<*>))


-- | Construct a stateless vertex that sends each input to every 'Edge' in the output list.
--
-- @since 0.1.3.0
fanout :: [Edge a] -> Dataflow (Edge a)
fanout nexts = statelessVertex $ \timestamp x -> mapM_ (\next -> send next timestamp x) nexts

-- | Construct a stateless vertex that applies the provided function to every input
-- and sends the result to the output.
--
-- @since 0.1.3.0
map :: (i -> o) -> Edge o -> Dataflow (Edge i)
map f next = statelessVertex $ \timestamp x -> send next timestamp (f x)

-- | Construct a stateful vertex with two input edges.
--
-- @since 0.1.3.0
join2 ::
  state
  -> (StateRef state -> Timestamp -> i -> Dataflow ())
  -> (StateRef state -> Timestamp -> j -> Dataflow ())
  -> (StateRef state -> Timestamp -> Dataflow ())
  -> Dataflow (Edge i, Edge j)
join2 initState callbackI callbackJ finalizer = do
  stateRef <- newState initState

  registerFinalizer $ finalizer stateRef

  (,) <$> registerVertex (StatefulVertex stateRef callbackI)
      <*> registerVertex (StatefulVertex stateRef callbackJ)

-- | Construct a stateful vertex with three input edges.
--
-- @since 0.1.3.0
join3 ::
  state
  -> (StateRef state -> Timestamp -> i -> Dataflow ())
  -> (StateRef state -> Timestamp -> j -> Dataflow ())
  -> (StateRef state -> Timestamp -> k -> Dataflow ())
  -> (StateRef state -> Timestamp -> Dataflow ())
  -> Dataflow (Edge i, Edge j, Edge k)
join3 initState callbackI callbackJ callbackK finalizer = do
  stateRef <- newState initState

  registerFinalizer $ finalizer stateRef

  (,,) <$> registerVertex (StatefulVertex stateRef callbackI)
       <*> registerVertex (StatefulVertex stateRef callbackJ)
       <*> registerVertex (StatefulVertex stateRef callbackK)
