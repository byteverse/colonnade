{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Colonnade.Types
  ( Encoding(..)
  , Decoding(..)
  , OneEncoding(..)
  , Headed(..)
  , Headless(..)
  , Indexed(..)
  , HeadingErrors(..)
  , DecodingCellError(..)
  , DecodingRowError(..)
  , DecodingCellErrors(..)
  , RowError(..)
  ) where

import Data.Vector (Vector)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Functor.Contravariant.Divisible (Divisible(..))
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import qualified Data.Vector as Vector

-- | Isomorphic to 'Identity'
newtype Headed a = Headed { getHeaded :: a }
  deriving (Eq,Ord,Functor,Show,Read)

-- | Isomorphic to 'Proxy'
data Headless a = Headless
  deriving (Eq,Ord,Functor,Show,Read)

data Indexed f a = Indexed
  { indexedIndex :: !Int
  , indexedHeading :: !(f a)
  } deriving (Eq,Ord,Functor,Show,Read)

data HeadingErrors content = HeadingErrors
  { headingErrorsMissing   :: Vector content       -- ^ headers that were missing
  , headingErrorsDuplicate :: Vector (content,Int) -- ^ headers that occurred more than once
  } deriving (Show,Read)

instance (Show content, Typeable content) => Exception (HeadingErrors content)

instance Monoid (HeadingErrors content) where
  mempty = HeadingErrors Vector.empty Vector.empty
  mappend (HeadingErrors a1 b1) (HeadingErrors a2 b2) = HeadingErrors
    (a1 Vector.++ a2) (b1 Vector.++ b2)

data DecodingCellError f content = DecodingCellError
  { decodingCellErrorContent :: !content
  , decodingCellErrorHeader  :: !(Indexed f content)
  , decodingCellErrorMessage :: !String
  } deriving (Show,Read)

-- instance (Show (f content), Typeable content) => Exception (DecodingError f content)

newtype DecodingCellErrors f content = DecodingCellErrors
  { getDecodingCellErrors :: Vector (DecodingCellError f content)
  } deriving (Monoid,Show,Read)

-- newtype ParseRowError = ParseRowError String

data DecodingRowError f content = DecodingRowError
  { decodingRowErrorRow   :: !Int
  , decodingRowErrorError :: !(RowError f content)
  }

data RowError f content
  = RowErrorParse !String -- ^ Error occurred parsing the document into cells
  | RowErrorDecode !(DecodingCellErrors f content) -- ^ Error decoding the content
  | RowErrorSize !Int !Int -- ^ Wrong number of cells in the row
  | RowErrorHeading !(HeadingErrors content)
  | RowErrorMinSize !Int !Int

-- instance (Show (f content), Typeable content) => Exception (DecodingErrors f content)

instance Contravariant Headless where
  contramap _ Headless = Headless

-- | This just actually a specialization of the free applicative.
--   Check out @Control.Applicative.Free@ in the @free@ library to
--   learn more about this.
data Decoding f content a where
  DecodingPure :: !a -- ^ function
               -> Decoding f content a
  DecodingAp :: !(f content) -- ^ header
             -> !(content -> Either String a) -- ^ decoding function
             -> !(Decoding f content (a -> b)) -- ^ next decoding
             -> Decoding f content b

instance Functor (Decoding f content) where
  fmap f (DecodingPure a) = DecodingPure (f a)
  fmap f (DecodingAp h c apNext) = DecodingAp h c ((f .) <$> apNext)

instance Applicative (Decoding f content) where
  pure = DecodingPure
  DecodingPure f <*> y = fmap f y
  DecodingAp h c y <*> z = DecodingAp h c (flip <$> y <*> z)

data OneEncoding f content a = OneEncoding
  { oneEncodingHead   :: !(f content)
  , oneEncodingEncode :: !(a -> content)
  }

instance Contravariant (OneEncoding f content) where
  contramap f (OneEncoding h e) = OneEncoding h (e . f)

newtype Encoding f content a = Encoding
  { getEncoding :: Vector (OneEncoding f content a)
  } deriving (Monoid)

instance Contravariant (Encoding f content) where
  contramap f (Encoding v) = Encoding
    (Vector.map (contramap f) v)

instance Divisible (Encoding f content) where
  conquer = Encoding Vector.empty
  divide f (Encoding a) (Encoding b) =
    Encoding $ (Vector.++)
      (Vector.map (contramap (fst . f)) a)
      (Vector.map (contramap (snd . f)) b)
      -- (Vector.map (\(OneEncoding h c) -> (h,c . fst . f)) a)
      -- (Vector.map (\(OneEncoding h c) -> (h,c . snd . f)) b)

