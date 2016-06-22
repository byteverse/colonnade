{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Colonnade.Types
  ( Encoding(..)
  , Decoding(..)
  , Headed(..)
  , Headless(..)
  ) where

import Data.Vector (Vector)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Functor.Contravariant.Divisible (Divisible(..))
import qualified Data.Vector as Vector

-- | Isomorphic to 'Identity'
newtype Headed a = Headed { getHeaded :: a }
  deriving (Eq,Ord,Functor,Show,Read)

-- | Isomorphic to 'Proxy'
data Headless a = Headless
  deriving (Eq,Ord,Functor,Show,Read)

instance Contravariant Headless where
  contramap _ Headless = Headless

-- | This just actually a specialization of the free applicative.
--   Check out @Control.Applicative.Free@ in the @free@ library to
--   learn more about this.
data Decoding f content a where
  DecodingPure :: !a
               -> Decoding f content a
  DecodingAp :: !(f content)
             -> !(content -> Either String a)
             -> !(Decoding f content (a -> b))
             -> Decoding f content b

instance Functor (Decoding f content) where
  fmap f (DecodingPure a) = DecodingPure (f a)
  fmap f (DecodingAp h c apNext) = DecodingAp h c ((f .) <$> apNext)

instance Applicative (Decoding f content) where
  pure = DecodingPure
  DecodingPure f <*> y = fmap f y
  DecodingAp h c y <*> z = DecodingAp h c (flip <$> y <*> z)

newtype Encoding f content a = Encoding
  { getEncoding :: Vector (f content,a -> content) }
  deriving (Monoid)

instance Contravariant (Encoding f content) where
  contramap f (Encoding v) = Encoding
    (Vector.map (\(h,c) -> (h, c . f)) v)

instance Divisible (Encoding f content) where
  conquer = Encoding Vector.empty
  divide f (Encoding a) (Encoding b) =
    Encoding $ (Vector.++)
      (Vector.map (\(h,c) -> (h,c . fst . f)) a)
      (Vector.map (\(h,c) -> (h,c . snd . f)) b)


