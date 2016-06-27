module Siphon.Types where

import Data.Vector (Vector)
import qualified Data.Attoparsec.Types as Atto

newtype Escaped c = Escaped { getEscaped :: c }

-- | Consider changing out the use of 'Vector' here
-- with the humble list instead. It might fuse away
-- better. Not sure though.
data Siphon c1 c2 = Siphon
  { siphonEscape :: !(c1 -> Escaped c2)
  , siphonIntercalate :: !(Vector (Escaped c2) -> c2)
  }

data SiphonDecoding c1 c2 = SiphonDecoding
  { siphonDecodingParse :: c1 -> Atto.IResult c1 (Vector c2)
  , siphonDecodingNull  :: c1 -> Bool
  }

data WithEnd c = WithEnd
  { withEndEnded :: Bool
  , withEndContent :: c
  }

-- data SiphonDecodingError
--   { clarify 
--   }

