module FormalConceptAnalysis.Internal.Internal where

import qualified Data.Vector as V
import Data.List ( intersect, nub )


intersectVec :: (Eq a) => V.Vector a -> V.Vector a -> V.Vector a
intersectVec xs ys =
    V.fromList $ intersect (V.toList xs) (V.toList ys)


intersects :: (Eq a) => V.Vector a -> V.Vector (V.Vector a) -> V.Vector a
intersects = V.foldl intersectVec 


unionVec :: (Eq a) => V.Vector a -> V.Vector a -> V.Vector a
unionVec xs ys =
    V.fromList $ nub (V.toList xs ++ V.toList ys)