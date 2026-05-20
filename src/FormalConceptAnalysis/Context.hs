module FormalConceptAnalysis.Context
    ( Object
    , Attribute
    , Objects
    , Attributes
    , Incidence
    , Context (..)
    , mkContext
    , contextRows
    , row
    , column
    ) where

import Data.List (intercalate, transpose)
import qualified Data.Vector as V
import Data.Maybe (fromJust)

type Object = String
type Attribute = String
type Objects = V.Vector Object
type Attributes = V.Vector Attribute
type Incidence = V.Vector (V.Vector Bool)

data Context = Context
    { objects :: Objects
    , attributes :: Attributes
    , incidence :: Incidence
    }
    deriving (Eq)

mkContext :: Objects -> Attributes -> Incidence -> Either String Context
mkContext objs attrs inc
    | V.length inc /= V.length objs =
        Left "Incidence row count must match the number of objects."
    | not (allRowsMatchAttributeCount inc attributeCount) =
        Left "Each incidence row must have the same length as the number of attributes."
    | otherwise =
        Right
            Context
                { objects = objs
                , attributes = attrs
                , incidence = inc
                }
    where attributeCount = V.length attrs

instance Show Context where
    show = formatMatrix . contextRows


contextRows :: Context -> [[String]]
contextRows context = headerRow : dataRows
    where
        objectNames = V.toList (objects context)
        attributeNames = V.toList (attributes context)
        incidenceRows = V.toList (incidence context)
        headerRow = "" : attributeNames
        dataRows = zipWith renderRow objectNames incidenceRows
        renderRow objectName incidenceRow = objectName : map boolToCell (V.toList incidenceRow)


boolToCell :: Bool -> String
boolToCell True = "1"
boolToCell False = "0"

allRowsMatchAttributeCount :: Incidence -> Int -> Bool
allRowsMatchAttributeCount inc attributeCount =
    V.all ((== attributeCount) . V.length) inc


formatMatrix :: [[String]] -> String
formatMatrix rows = unlines (map renderRow rows)
  where
    columnWidths = [ maximum (map length currentColumn)| currentColumn <- transpose rows]
    renderRow currentRow = intercalate " | " (zipWith padRight columnWidths currentRow)
    padRight width value = value ++ replicate (width - length value) ' '


row :: Context -> Object -> Attributes
row c obj = V.map fst $ V.filter snd (V.zip (attributes c) rowRelation) 
    where 
        idx = fromJust $ V.elemIndex obj (objects c)
        rowRelation = incidence c V.! idx
        

column :: Context -> Attribute -> Objects
column c attr = V.map fst $ V.filter snd (V.zip (objects c) colRelation) 
    where 
        idx = fromJust $ V.elemIndex attr (attributes c)
        colRelation = V.map (V.! idx) (incidence c)
