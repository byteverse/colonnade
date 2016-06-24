{-# LANGUAGE DeriveFunctor #-}
module Colonnade.Internal where

newtype EitherWrap a b = EitherWrap
  { getEitherWrap :: Either a b
  } deriving (Functor)

instance Monoid a => Applicative (EitherWrap a) where
  pure = EitherWrap . Right
  EitherWrap (Left a1) <*> EitherWrap (Left a2) = EitherWrap (Left (mappend a1 a2))
  EitherWrap (Left a1) <*> EitherWrap (Right _) = EitherWrap (Left a1)
  EitherWrap (Right _) <*> EitherWrap (Left a2) = EitherWrap (Left a2)
  EitherWrap (Right f) <*> EitherWrap (Right b) = EitherWrap (Right (f b))

