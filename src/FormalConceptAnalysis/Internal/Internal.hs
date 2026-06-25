module FormalConceptAnalysis.Internal.Internal where

import Data.List ( intersect, nub )
import qualified Data.Vector as V


intersectVec :: (Eq a) => V.Vector a -> V.Vector a -> V.Vector a
intersectVec xs ys =
    V.fromList $ intersect (V.toList xs) (V.toList ys)


intersects :: (Eq a) => V.Vector a -> V.Vector (V.Vector a) -> V.Vector a
intersects = V.foldl intersectVec 


unionVec :: (Eq a) => V.Vector a -> V.Vector a -> V.Vector a
unionVec xs ys =
    V.fromList $ nub (V.toList xs ++ V.toList ys)


subsets :: [a] -> [[a]]
subsets = foldr step [[]]
     where step x = concatMap (\ys -> [ys, x : ys])


subsetsVec :: V.Vector a -> V.Vector (V.Vector a)
subsetsVec =
    V.foldr step (V.singleton V.empty)
  where
    step x acc =
        acc V.++ V.map (V.cons x) acc
