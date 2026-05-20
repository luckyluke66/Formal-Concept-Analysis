module FormalConceptAnalysis.Concept(
    Concept(..)
    ,mkConcept
    ,fromAttr
    ,fromObj
    ,fromExtent
    ,fromIntent
    ,(\/)
    ,(/\)
) where 

import FormalConceptAnalysis.Context
import FormalConceptAnalysis.Internal.Internal


data Concept = Concept Objects Attributes
    deriving (Eq)


(/\) :: Context -> Concept -> Concept -> Concept
(/\) c (Concept a b) (Concept a' b') = Concept (intersectVec a a')  (closeAttr c (unionVec b b'))


(\/) :: Context -> Concept -> Concept -> Concept
(\/) c (Concept a b) (Concept a' b') = Concept (closeObj c (unionVec a a')) (intersectVec b b')


mkConcept :: [Object] -> [Attribute] -> Concept
mkConcept o a = Concept (mkObjs o) (mkAttrs a)


fromObj :: Context -> Objects -> Concept
fromObj c objs = Concept ext int
    where 
        int = up c ext
        ext = closeObj c objs


fromAttr :: Context -> Attributes -> Concept
fromAttr c attrs = Concept ext int
    where 
        int = closeAttr c attrs
        ext = down c int


fromExtent :: Context -> Objects -> Concept
fromExtent c ext = Concept ext (up c ext)


fromIntent :: Context -> Attributes -> Concept
fromIntent c int = Concept (down c int) int 