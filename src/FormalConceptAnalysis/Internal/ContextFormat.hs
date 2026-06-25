module FormalConceptAnalysis.Internal.ContextFormat
    ( contextRowsFromParts
    , formatContextFromParts
    ) where

import Data.List (intercalate, transpose)
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M

contextRowsFromParts :: V.Vector String -> V.Vector String -> Map (String, String) Bool -> [[String]]
contextRowsFromParts objectNames attributeNames incidenceMap = headerRow : dataRows
    where
        headerRow = "" : V.toList attributeNames
        attrs = V.toList attributeNames
        objs = V.toList objectNames
        dataRows = map renderRow objs

        renderRow objectName =
            objectName : map (\attr -> boolToCell (M.findWithDefault False (objectName, attr) incidenceMap)) attrs

boolToCell :: Bool -> String
boolToCell True = "1"
boolToCell False = "0"

formatMatrix :: [[String]] -> String
formatMatrix rows = unlines (map renderRow rows)
  where
    columnWidths = [maximum (map length currentColumn) | currentColumn <- transpose rows]
    renderRow currentRow = intercalate " | " (zipWith padRight columnWidths currentRow)
    padRight width value = value ++ replicate (width - length value) ' '

formatContextFromParts :: V.Vector String -> V.Vector String -> Map (String, String) Bool -> String
formatContextFromParts objectNames attributeNames incidenceMap =
  formatMatrix (contextRowsFromParts objectNames attributeNames incidenceMap)
