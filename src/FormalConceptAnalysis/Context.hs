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
    ) where


import FormalConceptAnalysis.Internal.Internal
import FormalConceptAnalysis.Internal.ContextFormat
import qualified Data.Vector as V
import Data.Map (Map, (!))


type Object = String
type Attribute = String
type Objects = V.Vector Object
type Attributes = V.Vector Attribute
type Incidence = (Map (Object, Attribute) Bool)


data Context = Context Objects Attributes Incidence
    deriving (Eq)

class FormalContext c where
    row :: c -> Object -> Attributes
    col :: c -> Attribute -> Objects
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
    row (Context _ a i) obj = V.filter (\x -> i ! (obj, x)) a
    col (Context o _ i) att = V.filter (\x -> i ! (x, att)) o
    up (Context o a i) objs = intersects a rows
        where rows = V.map (row (Context o a i)) objs
    down (Context o a i) attrs = intersects o cols
        where cols = V.map (col (Context o a i)) attrs

    objects (Context o _ _) = o
    attributes (Context _ a _) = a
    incidence (Context _ _ i) = i

mkObjs :: [a] -> V.Vector a
mkObjs = V.fromList


mkAttrs :: [a] -> V.Vector a
mkAttrs = V.fromList
