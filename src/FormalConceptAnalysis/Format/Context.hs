module FormalConceptAnalysis.Format.Context
    ( contextRows
    , formatContext
    ) where

import FormalConceptAnalysis.Context (Context (..))
import FormalConceptAnalysis.Internal.ContextFormat (contextRowsFromParts, formatContextFromParts)

contextRows :: Context -> [[String]]
contextRows context =
    contextRowsFromParts (objects context) (attributes context) (incidence context)

formatContext :: Context -> String
formatContext context =
    formatContextFromParts (objects context) (attributes context) (incidence context)
