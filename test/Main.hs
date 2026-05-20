module Main (main) where

import FormalConceptAnalysis.Format.CSV (fromCSVFile)
import FormalConceptAnalysis.Context
import qualified Data.Vector as V


mkContext' :: Either String Context -> Context
mkContext' (Left e) = error e
mkContext' (Right context) = context


main :: IO ()
main = do
    result <- fromCSVFile "test/data/sample-context.csv"
    let context = mkContext' result
    let up' = up context
    let down' = down context
    print context
    print $ down' $ up' (V.singleton "duck")
    print$ column context "swims"
