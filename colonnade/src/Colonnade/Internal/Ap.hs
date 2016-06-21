{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

module Colonnade.Internal.Ap
  ( Ap(..)
  , runAp
  , runAp_
  , liftAp
  , hoistAp
  , retractAp
  ) where

import Control.Applicative

-- | The free 'Applicative' for a 'Functor' @f@.
data Ap f a where
  Pure :: a -> Ap f a
  Ap   :: f a -> Ap f (a -> b) -> Ap f b

runAp :: Applicative g => (forall x. f x -> g x) -> Ap f a -> g a
runAp _ (Pure x) = pure x
runAp u (Ap f x) = flip id <$> u f <*> runAp u x

runAp_ :: Monoid m => (forall a. f a -> m) -> Ap f b -> m
runAp_ f = getConst . runAp (Const . f)

instance Functor (Ap f) where
  fmap f (Pure a)   = Pure (f a)
  fmap f (Ap x y)   = Ap x ((f .) <$> y)

instance Applicative (Ap f) where
  pure = Pure
  Pure f <*> y = fmap f y
  Ap x y <*> z = Ap x (flip <$> y <*> z)

liftAp :: f a -> Ap f a
liftAp x = Ap x (Pure id)
{-# INLINE liftAp #-}

hoistAp :: (forall a. f a -> g a) -> Ap f b -> Ap g b
hoistAp _ (Pure a) = Pure a
hoistAp f (Ap x y) = Ap (f x) (hoistAp f y)

retractAp :: Applicative f => Ap f a -> f a
retractAp (Pure a) = pure a
retractAp (Ap x y) = x <**> retractAp y
