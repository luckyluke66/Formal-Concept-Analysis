module FormalConceptAnalysis.Algorithm.NextClosure where

import FormalConceptAnalysis.Context
import FormalConceptAnalysis.Concept
import qualified Data.Vector as V
import qualified Data.Set as S


-- | Converts Attributes to a set of attribute indices.
attributesToSet :: FormalContext ctx => ctx -> Attributes -> S.Set Int
attributesToSet c ats = S.fromList [ i | i <- indices, attributes c V.! i `V.elem` ats]
    where
        indices = [0 .. V.length (attributes c) - 1]


-- | Converts a set of attribute indices back to Attributes.
setToAttributes :: FormalContext ctx => S.Set Int -> ctx -> Attributes
setToAttributes set c = mkAttrs (map (attributes c V.!) (S.toList set))


-- | Computes the closure of a candidate set.
candidateClosure :: FormalContext ctx => Int -> S.Set Int -> ctx -> S.Set Int
candidateClosure atr curr c = attributesToSet c (closeAttr c candidate)
    where
        smaller = S.filter (< atr) curr
        candidate = setToAttributes (S.insert atr smaller) c


-- | Checks whether a candidate satisfies the lectic condition.
isLecticCandidate :: Int -> S.Set Int -> S.Set Int -> Bool
isLecticCandidate atr curr next = S.filter (< atr) curr == S.filter (< atr) next


-- | Finds the next closed intent using the NextClosure algorithm.
nextClosure :: FormalContext ctx => S.Set Int -> S.Set Int -> ctx -> Maybe Attributes
nextClosure curr atrs c = tryCandidate (S.lookupMax atrs)
    where
        tryCandidate Nothing = Nothing
        tryCandidate (Just atr)
            | atr `S.member` curr = tryCandidate (S.lookupLT atr atrs)
            | isLecticCandidate atr curr next = Just (setToAttributes next c)
            | otherwise = tryCandidate (S.lookupLT atr atrs)
                where
                    next = candidateClosure atr curr c


-- | Generates all closed intents.
closedIntents :: FormalContext ctx => ctx -> V.Vector Attributes
closedIntents c = go firstIntent
    where
        atrs = S.fromList [0 .. V.length (attributes c) - 1]
        firstIntent = closeAttr c (mkAttrs [])
        go curr = curr `V.cons` maybe V.empty go (nextClosure (attributesToSet c curr) atrs c)


-- | Generates all formal concepts using the NextClosure algorithm.
allSubsets :: FormalContext ctx => ctx -> V.Vector Concept
allSubsets c = V.map (fromIntent c) (closedIntents c)