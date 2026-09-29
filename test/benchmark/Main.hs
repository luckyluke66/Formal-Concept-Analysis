module Main (main) where

import FormalConceptAnalysis.Format.CSV (fromCSVFile)
import FormalConceptAnalysis.Context

import qualified FormalConceptAnalysis.Algorithm.Naive as Naive
import qualified FormalConceptAnalysis.Algorithm.NextClosure as NextClosure
import qualified Data.Vector as V


-- Forces evaluation so the benchmark measures the actual computation time, since Haskell is lazy.
import Control.Exception (evaluate) 
import Data.Time.Clock (getCurrentTime, diffUTCTime)


mkContext' :: Either String Context -> Context
mkContext' (Left e) = error e
mkContext' (Right context) = context


measure :: String -> IO Int -> IO ()
measure name action = do
    start <- getCurrentTime
    count <- action
    end <- getCurrentTime

    putStrLn $ name ++ ":"
    putStrLn $ "  concepts: " ++ show count
    putStrLn $ "  time: " ++ show (diffUTCTime end start)


main :: IO ()
main = do
    result <- fromCSVFile "test/data/benchmark-20x10.csv"
    let context = mkContext' result

    putStrLn "Algorithm comparison"
    putStrLn "____________________"

    measure "Naive" $
        evaluate (V.length (Naive.allSubsets context))

    measure "NextClosure" $
        evaluate (V.length (NextClosure.allSubsets context))