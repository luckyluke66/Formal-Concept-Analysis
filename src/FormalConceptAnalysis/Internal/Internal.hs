module FormalConceptAnalysis.Internal.Internal where

import qualified Data.Vector as V

intersect :: V.Vector a -> V.Vector a -> V.Vector a
intersect v1 v2 = (concat v1 v2)