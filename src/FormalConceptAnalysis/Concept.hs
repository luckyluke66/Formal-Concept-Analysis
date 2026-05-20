module FormalConceptAnalysis.Concept(
    Concept(..)
    ,mkConcept
    ,mkFromAttr
    ,mkFromObj
) where 

import FormalConceptAnalysis.Context

data Concept = Concept Objects Attributes

mkConcept :: [Object] -> [Attribute] -> Concept
mkConcept o a = Concept (mkObjs o) (mkAttrs a)

mkFromObj :: Context -> Objects -> Concept
mkFromObj c objs = Concept ext int
    where 
        int = up c ext
        ext = closeObj c objs

mkFromAttr :: Context -> Attributes -> Concept
mkFromAttr c attrs = Concept ext int
    where 
        int = closeAttr c attrs
        ext = down c int