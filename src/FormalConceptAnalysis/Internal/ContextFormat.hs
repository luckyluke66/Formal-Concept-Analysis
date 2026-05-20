module FormalConceptAnalysis.Internal.ContextFormat
    ( contextRowsFromParts
    , formatContextFromParts
    ) where

import Data.List (intercalate, transpose)
import qualified Data.Vector as V

contextRowsFromParts :: V.Vector String -> V.Vector String -> V.Vector (V.Vector Bool) -> [[String]]
contextRowsFromParts objectNames attributeNames incidenceRows = headerRow : dataRows
  where
    headerRow = "" : V.toList attributeNames
    dataRows = zipWith renderRow (V.toList objectNames) (V.toList incidenceRows)

    renderRow objectName incidenceRow =
        objectName : map boolToCell (V.toList incidenceRow)

boolToCell :: Bool -> String
boolToCell True = "1"
boolToCell False = "0"

formatMatrix :: [[String]] -> String
formatMatrix rows = unlines (map renderRow rows)
  where
    columnWidths = [maximum (map length currentColumn) | currentColumn <- transpose rows]
    renderRow currentRow = intercalate " | " (zipWith padRight columnWidths currentRow)
    padRight width value = value ++ replicate (width - length value) ' '

formatContextFromParts :: V.Vector String -> V.Vector String -> V.Vector (V.Vector Bool) -> String
formatContextFromParts objectNames attributeNames incidenceRows =
    formatMatrix (contextRowsFromParts objectNames attributeNames incidenceRows)
