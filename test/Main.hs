module Main (main) where

import FormalConceptAnalysis.Format.CSV (fromCSVFile)
import FormalConceptAnalysis.Format.Lattice (formatCoverRelation, formatLevels)
import FormalConceptAnalysis.Context
import FormalConceptAnalysis.Algorithm.Naive


mkContext' :: Either String Context -> Context
mkContext' (Left e) = error e
mkContext' (Right context) = context


main :: IO ()
main = do
    result <- fromCSVFile "test/data/sample-context.csv"
    let context = mkContext' result
    let concepts = allSubsets context

    print context
    print "_______________"
    putStrLn $ formatLevels concepts
    putStrLn $ formatCoverRelation concepts

