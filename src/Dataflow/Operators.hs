module Dataflow.Operators (
  fanout,
  join3
) where

import           Data.Dynamic        (Typeable)
import           Dataflow.Primitives (Dataflow, Edge, StateRef, Timestamp,
                                      Vertex (StatefulVertex), newState,
                                      registerVertex, send)
import           Dataflow.Vertices   (statelessVertex)
import           Prelude             (mapM_, ($), (<$>), (<*>))


fanout :: Typeable a => [Edge a] -> Dataflow (Edge a)
fanout nexts = statelessVertex $ \timestamp x -> mapM_ (\next -> send next timestamp x) nexts


join3 :: (Typeable i, Typeable j, Typeable k) =>
  s
  -> (StateRef s -> Timestamp -> i -> Dataflow ())
  -> (StateRef s -> Timestamp -> j -> Dataflow ())
  -> (StateRef s -> Timestamp -> k -> Dataflow ())
  -> Dataflow (Edge i, Edge j, Edge k)
join3 initState callbackI callbackJ callbackK = do
  stateRef <- newState initState

  (,,) <$> registerVertex (StatefulVertex stateRef callbackI)
       <*> registerVertex (StatefulVertex stateRef callbackJ)
       <*> registerVertex (StatefulVertex stateRef callbackK)
