module FormalConceptAnalysis.Format.Context
    ( contextRows
    , formatContext
    ) where

import FormalConceptAnalysis.Context (Context (..))
import FormalConceptAnalysis.Internal.ContextFormat (contextRowsFromParts, formatContextFromParts)

contextRows :: Context -> [[String]]
contextRows (Context objs attrs inc) =
    contextRowsFromParts objs attrs inc

formatContext :: Context -> String
formatContext (Context objs attrs inc) =
    formatContextFromParts objs attrs inc
