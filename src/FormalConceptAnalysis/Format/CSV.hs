module FormalConceptAnalysis.Format.CSV
    ( fromCSV
    , fromCSVFile
    , toCsv
    ) where

import Data.Char (isSpace, toLower)
import Data.List (intercalate)
import qualified Data.Vector as V
import FormalConceptAnalysis.Context

fromCSVFile :: FilePath -> IO (Either String Context)
fromCSVFile path = fromCSV <$> readFile path

toCsv :: Context -> String
toCsv = unlines . map (intercalate ",") . contextRows

fromCSV :: String -> Either String Context
fromCSV input =
    case filter (not . all null) (map parseRow (lines input)) of
        [] -> Left "CSV is empty."
        (header : rows) -> buildContext header rows

buildContext :: [String] -> [[String]] -> Either String Context
buildContext header rows
    | length header < 2 = Left "CSV header must contain an empty corner cell and at least one attribute."
    | null attrs = Left "CSV must define at least one attribute."
    | otherwise =
        mapM parseObjectRow (zip [2 ..] rows) >>= toContext
  where
    attrs = drop 1 header

    parseObjectRow :: (Int, [String]) -> Either String (String, [String])
    parseObjectRow (lineNumber, cellsWithObject)
        | length cellsWithObject /= length header =
            Left $
                "Row "
                    ++ show lineNumber
                    ++ " has "
                    ++ show (length cellsWithObject)
                    ++ " columns, but the header has "
                    ++ show (length header)
                    ++ "."
        | null objectName =
            Left $ "Row " ++ show lineNumber ++ " is missing an object name in the first column."
        | otherwise =
            Right (objectName, cells)
      where
        objectName = head cellsWithObject
        cells = tail cellsWithObject

    toContext :: [(String, [String])] -> Either String Context
    toContext objectRows =
        mkContext objectNames attributeNames incidenceMatrix
      where
        objectNames = V.fromList (map fst objectRows)
        attributeNames = V.fromList attrs
        incidenceMatrix =
            V.fromList
                [ V.fromList (map isMarked cells)
                | (_, cells) <- objectRows
                ]

parseRow :: String -> [String]
parseRow = map trim . splitComma

splitComma :: String -> [String]
splitComma [] = [""]
splitComma (',' : rest) = "" : splitComma rest
splitComma (char : rest) =
    case splitComma rest of
        [] -> [[char]]
        (cell : cells) -> (char : cell) : cells

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd predicate = reverse . dropWhile predicate . reverse

isMarked :: String -> Bool
isMarked cell =
    map toLower cell `elem` ["1", "x", "true", "yes", "*"]
