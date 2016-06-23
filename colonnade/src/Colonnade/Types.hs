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
  , HeadingError(..)
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

-- | Isomorphic to @'Const' 'Int'@
newtype Indexed a = Indexed { getIndexed :: Int }
  deriving (Eq,Ord,Functor,Show,Read)

data HeadingError content = HeadingError
  { headingErrorMissing   :: Vector content       -- ^ headers that were missing
  , headingErrorDuplicate :: Vector (content,Int) -- ^ headers that occurred more than once
  } deriving (Show,Read)

instance (Show content, Typeable content) => Exception (HeadingError content)

instance Monoid (HeadingError content) where
  mempty = HeadingError Vector.empty Vector.empty
  mappend (HeadingError a1 b1) (HeadingError a2 b2) = HeadingError
    (a1 Vector.++ a2) (b1 Vector.++ b2)

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
  { getEncoding :: Vector (OneEncoding f content a) }
  deriving (Monoid)

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

