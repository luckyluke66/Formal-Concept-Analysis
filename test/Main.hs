module Main (main) where

import FormalConceptAnalysis.Format.CSV (fromCSVFile)
import FormalConceptAnalysis.Context


mkContext' :: Either String Context -> Context
mkContext' (Left e) = error e
mkContext' (Right context) = context


main :: IO ()
main = do
    result <- fromCSVFile "test/data/sample-context.csv"
    let context = mkContext' result

    print $ row context "duck"
    print$ column context "swims"
