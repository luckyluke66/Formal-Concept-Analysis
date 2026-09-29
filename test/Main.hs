module Main (main) where

import FormalConceptAnalysis.Format.CSV (fromCSVFile)
import FormalConceptAnalysis.Format.Lattice (formatCoverRelation, formatLevels)
import FormalConceptAnalysis.Context

import qualified FormalConceptAnalysis.Algorithm.Naive as Naive
import qualified FormalConceptAnalysis.Algorithm.NextClosure as NextClosure

mkContext' :: Either String Context -> Context
mkContext' (Left e) = error e
mkContext' (Right context) = context


main :: IO ()
main = do
    result <- fromCSVFile "test/data/sample-context.csv"
    let context = mkContext' result
    let concepts = NextClosure.allSubsets context

    print context
    print "_______________"
    putStrLn $ formatLevels concepts
    putStrLn $ formatCoverRelation concepts

