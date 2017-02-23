{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Siphon.Types where

import Data.Vector (Vector)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import qualified Data.Vector as Vector
import qualified Data.Attoparsec.Types as Atto

newtype Escaped c = Escaped { getEscaped :: c }

data Siphon c = Siphon
  { siphonEscape      :: !(c -> Escaped c)
  , siphonIntercalate :: !(Vector (Escaped c) -> c)
  , siphonParseRow    :: c -> Atto.IResult c (Vector c)
  , siphonNull        :: c -> Bool
  }

data DecolonnadeCellError f content = DecolonnadeCellError
  { decodingCellErrorContent :: !content
  , decodingCellErrorHeader  :: !(Indexed f content)
  , decodingCellErrorMessage :: !String
  } deriving (Show,Read,Eq)

-- instance (Show (f content), Typeable content) => Exception (DecolonnadeError f content)

data Indexed f a = Indexed
  { indexedIndex :: !Int
  , indexedHeading :: !(f a)
  } deriving (Eq,Ord,Functor,Show,Read)

newtype DecolonnadeCellErrors f content = DecolonnadeCellErrors
  { getDecolonnadeCellErrors :: Vector (DecolonnadeCellError f content)
  } deriving (Monoid,Show,Read,Eq)

-- newtype ParseRowError = ParseRowError String

-- TODO: rewrite the instances for this by hand. They
-- currently use FlexibleContexts.
data DecolonnadeRowError f content = DecolonnadeRowError
  { decodingRowErrorRow   :: !Int
  , decodingRowErrorError :: !(RowError f content)
  } deriving (Show,Read,Eq)

-- TODO: rewrite the instances for this by hand. They
-- currently use FlexibleContexts.
data RowError f content
  = RowErrorParse !String -- ^ Error occurred parsing the document into cells
  | RowErrorDecode !(DecolonnadeCellErrors f content) -- ^ Error decoding the content
  | RowErrorSize !Int !Int -- ^ Wrong number of cells in the row
  | RowErrorHeading !(HeadingErrors content)
  | RowErrorMinSize !Int !Int
  | RowErrorMalformed !String -- ^ Error decoding unicode content
  deriving (Show,Read,Eq)

data HeadingErrors content = HeadingErrors
  { headingErrorsMissing   :: Vector content       -- ^ headers that were missing
  , headingErrorsDuplicate :: Vector (content,Int) -- ^ headers that occurred more than once
  } deriving (Show,Read,Eq)

instance (Show content, Typeable content) => Exception (HeadingErrors content)

instance Monoid (HeadingErrors content) where
  mempty = HeadingErrors Vector.empty Vector.empty
  mappend (HeadingErrors a1 b1) (HeadingErrors a2 b2) = HeadingErrors
    (a1 Vector.++ a2) (b1 Vector.++ b2)

-- | This just actually a specialization of the free applicative.
--   Check out @Control.Applicative.Free@ in the @free@ library to
--   learn more about this. The meanings of the fields are documented
--   slightly more in the source code. Unfortunately, haddock does not
--   play nicely with GADTs.
data Decolonnade f content a where
  DecolonnadePure :: !a -- function
               -> Decolonnade f content a
  DecolonnadeAp :: !(f content) -- header
             -> !(content -> Either String a) -- decoding function
             -> !(Decolonnade f content (a -> b)) -- next decoding
             -> Decolonnade f content b

instance Functor (Decolonnade f content) where
  fmap f (DecolonnadePure a) = DecolonnadePure (f a)
  fmap f (DecolonnadeAp h c apNext) = DecolonnadeAp h c ((f .) <$> apNext)

instance Applicative (Decolonnade f content) where
  pure = DecolonnadePure
  DecolonnadePure f <*> y = fmap f y
  DecolonnadeAp h c y <*> z = DecolonnadeAp h c (flip <$> y <*> z)

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

