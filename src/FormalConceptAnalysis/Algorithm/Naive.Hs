module FormalConceptAnalysis.Algorithm.Naive where

import FormalConceptAnalysis.Context
import FormalConceptAnalysis.Concept
import FormalConceptAnalysis.Internal.Internal
import qualified Data.Vector as V


allSubsets :: Context -> V.Vector Concept
allSubsets c = V.map (fromExtent c) closedExtents
    where
        candidateExtents = subsetsVec (objects c)
        closedExtents = V.filter (\ext -> ext == closeObj c ext) candidateExtents
