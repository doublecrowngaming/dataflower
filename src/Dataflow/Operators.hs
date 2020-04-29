module Dataflow.Operators (
  fanout
) where

import           Data.Dynamic        (Typeable)
import           Dataflow.Primitives (Dataflow, Edge, send)
import           Dataflow.Vertices   (statelessVertex)
import           Prelude             (mapM_, ($))


fanout :: Typeable a => [Edge a] -> Dataflow (Edge a)
fanout nexts = statelessVertex $ \timestamp x -> mapM_ (\next -> send next timestamp x) nexts
