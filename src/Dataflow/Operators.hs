module Dataflow.Operators (
  fanout,
  map,
  join3
) where

import           Data.Dynamic        (Typeable)
import           Dataflow.Primitives (Dataflow, Edge, StateRef, Timestamp,
                                      Vertex (StatefulVertex), newState,
                                      registerFinalizer, registerVertex, send)
import           Dataflow.Vertices   (statelessVertex)
import           Prelude             (mapM_, ($), (<$>), (<*>))


fanout :: Typeable a => [Edge a] -> Dataflow (Edge a)
fanout nexts = statelessVertex $ \timestamp x -> mapM_ (\next -> send next timestamp x) nexts


map :: (Typeable i, Typeable o) => (i -> o) -> Edge o -> Dataflow (Edge i)
map f next = statelessVertex $ \timestamp x -> send next timestamp (f x)


join3 :: (Typeable i, Typeable j, Typeable k) =>
  s
  -> (StateRef s -> Timestamp -> i -> Dataflow ())
  -> (StateRef s -> Timestamp -> j -> Dataflow ())
  -> (StateRef s -> Timestamp -> k -> Dataflow ())
  -> (StateRef s -> Timestamp -> Dataflow ())
  -> Dataflow (Edge i, Edge j, Edge k)
join3 initState callbackI callbackJ callbackK finalizer = do
  stateRef <- newState initState

  registerFinalizer (finalizer stateRef)

  (,,) <$> registerVertex (StatefulVertex stateRef callbackI)
       <*> registerVertex (StatefulVertex stateRef callbackJ)
       <*> registerVertex (StatefulVertex stateRef callbackK)
