module Siphon.Types where

import Data.Vector (Vector)
import Colonnade.Types (DecodingRowError)
import qualified Data.Attoparsec.Types as Atto

newtype Escaped c = Escaped { getEscaped :: c }

data Siphon c = Siphon
  { siphonEscape      :: !(c -> Escaped c)
  , siphonIntercalate :: !(Vector (Escaped c) -> c)
  , siphonParseRow    :: c -> Atto.IResult c (Vector c)
  , siphonNull        :: c -> Bool
  }

-- -- | This type is provided for convenience with @pipes-text@
-- data CsvResult f c
--   = CsvResultSuccess
--   | CsvResultTextDecodeError
--   | CsvResultDecodeError (DecodingRowError f c)
--   deriving (Show,Read,Eq)


-- | Consider changing out the use of 'Vector' here
-- with the humble list instead. It might fuse away
-- better. Not sure though.
-- data SiphonX c1 c2 = SiphonX
--   { siphonXEscape :: !(c1 -> Escaped c2)
--   , siphonXIntercalate :: !(Vector (Escaped c2) -> c2)
--   }
--
-- data SiphonDecoding c1 c2 = SiphonDecoding
--   { siphonDecodingParse :: c1 -> Atto.IResult c1 (Vector c2)
--   , siphonDecodingNull  :: c1 -> Bool
--   }

-- data WithEnd c = WithEnd
--   { withEndEnded :: !Bool
--   , withEndContent :: !c
--   }

-- data SiphonDecodingError
--   { clarify
--   }

