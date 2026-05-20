module FormalConceptAnalysis.Context
    ( Object
    , Attribute
    , Objects
    , Attributes
    , Incidence
    , Context (..)
    , mkAttrs
    , mkObjs
    , mkContext
    , row
    , column
    , up
    , down
    , closeObj
    , closeAttr
    ) where


import FormalConceptAnalysis.Internal.Internal
import FormalConceptAnalysis.Internal.ContextFormat (formatContextFromParts)
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

instance Show Context where
    show (Context o a i) = formatContextFromParts o a i

mkObjs :: [a] -> V.Vector a
mkObjs = V.fromList


mkAttrs :: [a] -> V.Vector a
mkAttrs = V.fromList


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


allRowsMatchAttributeCount :: Incidence -> Int -> Bool
allRowsMatchAttributeCount inc attributeCount =
    V.all ((== attributeCount) . V.length) inc


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


up :: Context -> Objects -> Attributes
up c objs = intersects (attributes c) rows
    where rows = V.map (row c) objs


down :: Context -> Attributes -> Objects
down c attrs = intersects (objects c) cols
    where cols = V.map (column c) attrs


closeObj :: Context -> Objects -> Objects
closeObj c = down c . up c


closeAttr :: Context -> Objects -> Objects
closeAttr c = up c . down c
