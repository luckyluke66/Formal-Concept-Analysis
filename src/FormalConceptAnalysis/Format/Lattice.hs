module FormalConceptAnalysis.Format.Lattice
    ( formatLattice
    , formatLevels
    , formatCoverRelation
    ) where

import Data.List (intercalate, sortOn)
import Data.Ord (Down (..))
import qualified Data.Vector as V
import FormalConceptAnalysis.Concept

formatLattice :: V.Vector Concept -> String
formatLattice concepts =
    formatLevels concepts ++ "\n" ++ formatCoverRelation concepts

formatLevels :: V.Vector Concept -> String
formatLevels concepts =
    unlines ("Levels:" : concatMap renderLevel conceptGroups)
  where
    conceptList = sortOn (Down . conceptRank) (V.toList concepts)
    conceptGroups = groupedByRank conceptList

    renderLevel (rank, rankConcepts) =
        [ "  " ++ show rank ++ ":"
        , "    " ++ intercalate " ; " (map renderConcept rankConcepts)
        , ""
        ]

formatCoverRelation :: V.Vector Concept -> String
formatCoverRelation concepts =
    unlines $
        "Covers:"
            : case coverEdges conceptList of
                [] -> ["  <none>"]
                edges ->
                    [ "  " ++ renderConcept lower ++ " < " ++ renderConcept upper
                    | (lower, upper) <- edges
                    ]
  where
    conceptList = sortOn (Down . conceptRank) (V.toList concepts)

conceptRank :: Concept -> Int
conceptRank (Concept extent _) = V.length extent

groupedByRank :: [Concept] -> [(Int, [Concept])]
groupedByRank = foldr step []
  where
    step concept [] =
        [(conceptRank concept, [concept])]
    step concept ((rank, concepts) : rest)
        | conceptRank concept == rank =
            (rank, concept : concepts) : rest
        | otherwise =
            (conceptRank concept, [concept]) : (rank, concepts) : rest

coverEdges :: [Concept] -> [(Concept, Concept)]
coverEdges concepts =
    [ (lower, upper)
    | lower <- concepts
    , upper <- concepts
    , lower `isBelow` upper
    , not (hasIntermediate lower upper concepts)
    ]

isBelow :: Concept -> Concept -> Bool
isBelow (Concept lowerExtent _) (Concept upperExtent _) =
    lowerExtent /= upperExtent && isSubsetOf lowerExtent upperExtent

hasIntermediate :: Concept -> Concept -> [Concept] -> Bool
hasIntermediate lower upper =
    any (\middle -> lower `isBelow` middle && middle `isBelow` upper)

isSubsetOf :: Eq a => V.Vector a -> V.Vector a -> Bool
isSubsetOf xs ys = V.all (`V.elem` ys) xs

renderConcept :: Concept -> String
renderConcept (Concept extent intent) =
    "(" ++ renderVector extent ++ " | " ++ renderVector intent ++ ")"

renderVector :: V.Vector String -> String
renderVector values =
    "{" ++ intercalate ", " (V.toList values) ++ "}"
