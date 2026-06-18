module FormalConceptAnalysis.Context
    ( Object
    , Attribute
    , Objects
    , Attributes
    , Incidence
    , Context (..)
    , FormalContext (..)
    , mkAttrs
    , mkObjs
    , mkContext
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


data Context = Context Objects Attributes Incidence
    deriving (Eq)

class FormalContext c where
    row :: c -> Object -> Attributes
    column :: c -> Attribute -> Objects
    up :: c -> Objects -> Attributes
    down :: c -> Attributes -> Objects
    closeObj :: c -> Objects -> Objects
    closeObj ctx = down ctx . up ctx
    closeAttr :: c -> Attributes -> Attributes
    closeAttr ctx = up ctx . down ctx
    objects :: c -> Objects
    attributes :: c -> Attributes
    incidence :: c -> Incidence

instance Show Context where
    show (Context o a i) = formatContextFromParts o a i

instance FormalContext Context where
    row (Context o a i) obj = V.map fst $ V.filter snd (V.zip a rowRelation) 
        where 
            idx = fromJust $ V.elemIndex obj o
            rowRelation = i V.! idx

    column (Context o a i) attr = V.map fst $ V.filter snd (V.zip o colRelation) 
        where 
            idx = fromJust $ V.elemIndex attr a
            colRelation = V.map (V.! idx) i

    up (Context o a i) objs = intersects a rows
        where rows = V.map (row (Context o a i)) objs

    down (Context o a i) attrs = intersects o cols
        where cols = V.map (column (Context o a i)) attrs

    objects (Context o _ _) = o
    attributes (Context _ a _) = a
    incidence (Context _ _ i) = i

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
        Right (Context objs attrs inc)
    where attributeCount = V.length attrs


allRowsMatchAttributeCount :: Incidence -> Int -> Bool
allRowsMatchAttributeCount inc attributeCount =
    V.all ((== attributeCount) . V.length) inc
