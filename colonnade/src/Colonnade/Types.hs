{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs                      #-}
module Colonnade.Types
  ( Colonnade(..)
  , Decolonnade(..)
  , OneColonnade(..)
  , Headed(..)
  , Headless(..)
  , Indexed(..)
  , HeadingErrors(..)
  , DecolonnadeCellError(..)
  , DecolonnadeRowError(..)
  , DecolonnadeCellErrors(..)
  , RowError(..)
  ) where

import Data.Vector (Vector)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Functor.Contravariant.Divisible (Divisible(..))
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import qualified Data.Vector as Vector

-- | This type is isomorphic to 'Identity'.
newtype Headed a = Headed { getHeaded :: a }
  deriving (Eq,Ord,Functor,Show,Read,Foldable)

-- | This type is isomorphic to 'Proxy'
data Headless a = Headless
  deriving (Eq,Ord,Functor,Show,Read,Foldable)

data Indexed f a = Indexed
  { indexedIndex :: !Int
  , indexedHeading :: !(f a)
  } deriving (Eq,Ord,Functor,Show,Read)

data HeadingErrors content = HeadingErrors
  { headingErrorsMissing   :: Vector content       -- ^ headers that were missing
  , headingErrorsDuplicate :: Vector (content,Int) -- ^ headers that occurred more than once
  } deriving (Show,Read,Eq)

instance (Show content, Typeable content) => Exception (HeadingErrors content)

instance Monoid (HeadingErrors content) where
  mempty = HeadingErrors Vector.empty Vector.empty
  mappend (HeadingErrors a1 b1) (HeadingErrors a2 b2) = HeadingErrors
    (a1 Vector.++ a2) (b1 Vector.++ b2)

data DecolonnadeCellError f content = DecolonnadeCellError
  { decodingCellErrorContent :: !content
  , decodingCellErrorHeader  :: !(Indexed f content)
  , decodingCellErrorMessage :: !String
  } deriving (Show,Read,Eq)

-- instance (Show (f content), Typeable content) => Exception (DecolonnadeError f content)

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

-- instance (Show (f content), Typeable content) => Exception (DecolonnadeErrors f content)

instance Contravariant Headless where
  contramap _ Headless = Headless

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

-- | Encodes a header and a cell.
data OneColonnade f content a = OneColonnade
  { oneColonnadeHead   :: !(f content)
  , oneColonnadeEncode :: !(a -> content)
  }

instance Contravariant (OneColonnade f content) where
  contramap f (OneColonnade h e) = OneColonnade h (e . f)

-- | An columnar encoding of @a@. The type variable @f@ determines what
--   is present in each column in the header row. It is typically instantiated
--   to 'Headed' and occasionally to 'Headless'. There is nothing that
--   restricts it to these two types, although they satisfy the majority
--   of use cases. The type variable @c@ is the content type. This can
--   be @Text@, @String@, or @ByteString@. In the companion libraries
--   @reflex-dom-colonnade@ and @yesod-colonnade@, additional types
--   that represent HTML with element attributes are provided that serve
--   as the content type.
--
--   Internally, a 'Colonnade' is represented as a 'Vector' of individual
--   column encodings. It is possible to use any collection type with
--   'Alternative' and 'Foldable' instances. However, 'Vector' was chosen to
--   optimize the data structure for the use case of building the structure
--   once and then folding over it many times. It is recommended that
--   'Colonnade's are defined at the top-level so that GHC avoid reconstructing
--   them every time they are used.
newtype Colonnade f c a = Colonnade
  { getColonnade :: Vector (OneColonnade f c a)
  } deriving (Monoid)

instance Contravariant (Colonnade f content) where
  contramap f (Colonnade v) = Colonnade
    (Vector.map (contramap f) v)

instance Divisible (Colonnade f content) where
  conquer = Colonnade Vector.empty
  divide f (Colonnade a) (Colonnade b) =
    Colonnade $ (Vector.++)
      (Vector.map (contramap (fst . f)) a)
      (Vector.map (contramap (snd . f)) b)
      -- (Vector.map (\(OneEncoding h c) -> (h,c . fst . f)) a)
      -- (Vector.map (\(OneEncoding h c) -> (h,c . snd . f)) b)

