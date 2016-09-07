{-# LANGUAGE DeriveFunctor #-}
module Colonnade.Internal where

import Data.Foldable (foldrM)

newtype EitherWrap a b = EitherWrap
  { getEitherWrap :: Either a b
  } deriving (Functor)

instance Monoid a => Applicative (EitherWrap a) where
  pure = EitherWrap . Right
  EitherWrap (Left a1) <*> EitherWrap (Left a2) = EitherWrap (Left (mappend a1 a2))
  EitherWrap (Left a1) <*> EitherWrap (Right _) = EitherWrap (Left a1)
  EitherWrap (Right _) <*> EitherWrap (Left a2) = EitherWrap (Left a2)
  EitherWrap (Right f) <*> EitherWrap (Right b) = EitherWrap (Right (f b))

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft _ (Right a) = Right a
mapLeft f (Left a) = Left (f a)

foldMapM :: (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldMapM f = foldrM (\a b -> fmap (flip mappend b) (f a)) mempty

