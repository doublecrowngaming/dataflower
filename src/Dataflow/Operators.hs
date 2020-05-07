module Dataflow.Operators (
  fanout,
  map,
  join3
) where

import           Dataflow.Primitives (Dataflow, Edge, StateRef, Timestamp,
                                      Vertex (StatefulVertex), newState,
                                      registerFinalizer, registerVertex, send)
import           Dataflow.Vertices   (statelessVertex)
import           Prelude             (mapM_, ($), (<$>), (<*>))


fanout :: [Edge a] -> Dataflow (Edge a)
fanout nexts = statelessVertex $ \timestamp x -> mapM_ (\next -> send next timestamp x) nexts


map :: (i -> o) -> Edge o -> Dataflow (Edge i)
map f next = statelessVertex $ \timestamp x -> send next timestamp (f x)


join3 ::
  state
  -> (StateRef state -> Timestamp -> i -> Dataflow ())
  -> (StateRef state -> Timestamp -> j -> Dataflow ())
  -> (StateRef state -> Timestamp -> k -> Dataflow ())
  -> (StateRef state -> Timestamp -> Dataflow ())
  -> Dataflow (Edge i, Edge j, Edge k)
join3 initState callbackI callbackJ callbackK finalizer = do
  stateRef <- newState initState

  registerFinalizer (finalizer stateRef)

  (,,) <$> registerVertex (StatefulVertex stateRef callbackI)
       <*> registerVertex (StatefulVertex stateRef callbackJ)
       <*> registerVertex (StatefulVertex stateRef callbackK)
