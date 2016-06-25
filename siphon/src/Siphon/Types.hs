module Siphon.Types where

import Data.Vector (Vector)

newtype Escaped c = Escaped { getEscaped :: c }

-- | Consider changing out the use of 'Vector' here
-- with the humble list instead. It might fuse away
-- better. Not sure though.
data Siphon c1 c2 = Siphon
  { siphonEscape :: !(c1 -> Escaped c2)
  , siphonIntercalate :: !(Vector (Escaped c2) -> c2)
  }

-- data Clarify = Clarify
--   { clarify 
--   }
