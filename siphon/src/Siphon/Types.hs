{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Siphon.Types
  ( Siphon(..)
  , Indexed(..)
  , SiphonError(..)
  , RowError(..)
  , CellError(..)
  ) where

import Data.Vector (Vector)
import Control.Exception (Exception)
import Data.Text (Text)

data CellError = CellError
  { cellErrorColumn :: !Int
  , cellErrorContent :: !Text
  } deriving (Show,Read,Eq)

newtype Indexed a = Indexed
  { indexedIndex :: Int
  } deriving (Eq,Ord,Functor,Show,Read)

data SiphonError = SiphonError
  { siphonErrorRow :: !Int
  , siphonErrorCause :: !RowError
  } deriving (Show,Read,Eq)

instance Exception SiphonError

data RowError
  = RowErrorParse
    -- ^ Error occurred parsing the document into cells
  | RowErrorDecode !(Vector CellError)
    -- ^ Error decoding the content
  | RowErrorSize !Int !Int
    -- ^ Wrong number of cells in the row
  | RowErrorHeaders !(Vector (Vector CellError)) !(Vector Text) !(Vector Int)
    -- ^ Three parts:
    --   (a) Multiple header cells matched the same expected cell, 
    --   (b) Headers that were missing, 
    --   (c) Missing headers that were lambdas. They cannot be
    --   shown so instead their positions in the 'Siphon' are given.
  | RowErrorHeaderSize !Int !Int
    -- ^ Not enough cells in header, expected, actual
  | RowErrorMalformed !Int
    -- ^ Error decoding unicode content, column number
  deriving (Show,Read,Eq)

-- | This just actually a specialization of the free applicative.
--   Check out @Control.Applicative.Free@ in the @free@ library to
--   learn more about this. The meanings of the fields are documented
--   slightly more in the source code. Unfortunately, haddock does not
--   play nicely with GADTs.
data Siphon f c a where
  SiphonPure ::
       !a -- function
    -> Siphon f c a
  SiphonAp ::
       !(f c) -- header
    -> !(c -> Maybe a) -- decoding function
    -> !(Siphon f c (a -> b)) -- next decoding
    -> Siphon f c b

instance Functor (Siphon f c) where
  fmap f (SiphonPure a) = SiphonPure (f a)
  fmap f (SiphonAp h c apNext) = SiphonAp h c ((f .) <$> apNext)

instance Applicative (Siphon f c) where
  pure = SiphonPure
  SiphonPure f <*> y = fmap f y
  SiphonAp h c y <*> z = SiphonAp h c (flip <$> y <*> z)

