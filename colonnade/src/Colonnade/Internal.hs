{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-imports -fno-warn-unticked-promoted-constructors -Werror #-}

module Colonnade.Internal
  ( -- * Colonnade
    Colonnade(..)
  , OneColonnade(..)
  , Headed(..)
  , Headless(..)
    -- * Cornice
  , Cornice(..)
  , AnnotatedCornice(..)
  , OneCornice(..)
  , Pillar(..)
  , ToEmptyCornice(..)
  , Fascia(..)
    -- * Sizing
  , Sized(..)
  , MutableSizedColonnade(..)
  , MutableSizedCornice(..)
  ) where

import Data.Vector (Vector)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Functor.Contravariant.Divisible (Divisible(..))
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.Profunctor (Profunctor(..))
import Data.Semigroup (Semigroup)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Foldable (toList)
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Semigroup as Semigroup
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as VG

-- | As the first argument to the 'Colonnade' type 
--   constructor, this indictates that the columnar encoding has 
--   a header. This type is isomorphic to 'Identity' but is 
--   given a new name to clarify its intent:
--
-- > example :: Colonnade Headed Foo Text
--
--   The term @example@ represents a columnar encoding of @Foo@
--   in which the columns have headings.
newtype Headed a = Headed { getHeaded :: a }
  deriving (Eq,Ord,Functor,Show,Read,Foldable)

-- | As the first argument to the 'Colonnade' type 
--   constructor, this indictates that the columnar encoding does not have 
--   a header. This type is isomorphic to 'Proxy' but is 
--   given a new name to clarify its intent:
--
-- > example :: Colonnade Headless Foo Text
--
--   The term @example@ represents a columnar encoding of @Foo@
--   in which the columns do not have headings.
data Headless a = Headless
  deriving (Eq,Ord,Functor,Show,Read,Foldable)

data Sized f a = Sized
  { sizedSize :: {-# UNPACK #-} !Int
  , sizedContent :: !(f a)
  } deriving (Functor, Foldable)

instance Contravariant Headless where
  contramap _ Headless = Headless

-- | Encodes a header and a cell.
data OneColonnade h a c = OneColonnade
  { oneColonnadeHead   :: !(h c)
  , oneColonnadeEncode :: !(a -> c)
  } deriving (Functor)

instance Functor h => Profunctor (OneColonnade h) where
  rmap = fmap
  lmap f (OneColonnade h e) = OneColonnade h (e . f)

-- | An columnar encoding of @a@. The type variable @h@ determines what
--   is present in each column in the header row. It is typically instantiated
--   to 'Headed' and occasionally to 'Headless'. There is nothing that
--   restricts it to these two types, although they satisfy the majority
--   of use cases. The type variable @c@ is the content type. This can
--   be @Text@, @String@, or @ByteString@. In the companion libraries
--   @reflex-dom-colonnade@ and @yesod-colonnade@, additional types
--   that represent HTML with element attributes are provided that serve
--   as the content type. Presented more visually:
--
-- >             +---- Value consumed to build a row
-- >             |
-- >             v
-- > Colonnade h a c
-- >           ^   ^
-- >           |   |
-- >           |   +-- Content (Text, ByteString, Html, etc.)
-- >           |
-- >           +------ Headedness (Headed or Headless)
--
--   Internally, a 'Colonnade' is represented as a 'Vector' of individual
--   column encodings. It is possible to use any collection type with
--   'Alternative' and 'Foldable' instances. However, 'Vector' was chosen to
--   optimize the data structure for the use case of building the structure
--   once and then folding over it many times. It is recommended that
--   'Colonnade's are defined at the top-level so that GHC avoids reconstructing
--   them every time they are used.
newtype Colonnade h a c = Colonnade
  { getColonnade :: Vector (OneColonnade h a c)
  } deriving (Monoid,Functor)

instance Functor h => Profunctor (Colonnade h) where
  rmap = fmap
  lmap f (Colonnade v) = Colonnade (Vector.map (lmap f) v)

instance Semigroup (Colonnade h a c) where
  Colonnade a <> Colonnade b = Colonnade (a Vector.++ b)
  sconcat xs = Colonnade (vectorConcatNE (fmap getColonnade xs))

data MutableSizedColonnade s h a c = MutableSizedColonnade
  { mutableSizedColonnadeColumns :: {-# UNPACK #-} !(Vector (OneColonnade h a c))
  , mutableSizedColonnadeSizes :: {-# UNPACK #-} !(MVU.STVector s Int)
  }

-- | Isomorphic to the natural numbers. Only the promoted version of
--   this type is used.
data Pillar = Cap !Pillar | Base

class ToEmptyCornice (p :: Pillar) where
  toEmptyCornice :: Cornice p a c

instance ToEmptyCornice Base where
  toEmptyCornice = CorniceBase mempty

instance ToEmptyCornice (Cap p) where
  toEmptyCornice = CorniceCap Vector.empty

data Fascia (p :: Pillar) r where
  FasciaBase :: !r -> Fascia Base r
  FasciaCap :: !r -> Fascia p r -> Fascia (Cap p) r

data OneCornice k (p :: Pillar) a c = OneCornice
  { oneCorniceHead :: !c
  , oneCorniceBody :: !(k p a c)
  }

data Cornice (p :: Pillar) a c where
  CorniceBase :: !(Colonnade Headed a c) -> Cornice Base a c
  CorniceCap :: {-# UNPACK #-} !(Vector (OneCornice Cornice p a c)) -> Cornice (Cap p) a c

instance Semigroup (Cornice p a c) where
  CorniceBase a <> CorniceBase b = CorniceBase (mappend a b)
  CorniceCap a <> CorniceCap b = CorniceCap (a Vector.++ b)
  sconcat xs@(x :| _) = case x of
    CorniceBase _ -> CorniceBase (Colonnade (vectorConcatNE (fmap (getColonnade . getCorniceBase) xs)))
    CorniceCap _ -> CorniceCap (vectorConcatNE (fmap getCorniceCap xs))

instance ToEmptyCornice p => Monoid (Cornice p a c) where
  mempty = toEmptyCornice
  mappend = (Semigroup.<>)
  mconcat xs1 = case xs1 of
    [] -> toEmptyCornice
    x : xs2 -> Semigroup.sconcat (x :| xs2)

getCorniceBase :: Cornice Base a c -> Colonnade Headed a c
getCorniceBase (CorniceBase c) = c

getCorniceCap :: Cornice (Cap p) a c -> Vector (OneCornice Cornice p a c)
getCorniceCap (CorniceCap c) = c

data AnnotatedCornice (p :: Pillar) a c where
  AnnotatedCorniceBase :: !(Maybe Int) -> !(Colonnade (Sized Headed) a c) -> AnnotatedCornice Base a c
  AnnotatedCorniceCap :: 
       !(Maybe Int)
    -> {-# UNPACK #-} !(Vector (OneCornice AnnotatedCornice p a c))
    -> AnnotatedCornice (Cap p) a c

data MutableSizedCornice s (p :: Pillar) a c where
  MutableSizedCorniceBase :: 
       {-# UNPACK #-} !(MutableSizedColonnade s Headed a c) 
    -> MutableSizedCornice s Base a c
  MutableSizedCorniceCap :: 
       {-# UNPACK #-} !(Vector (OneCornice (MutableSizedCornice s) p a c))
    -> MutableSizedCornice s (Cap p) a c

-- data MaybeInt = JustInt {-# UNPACK #-} !Int | NothingInt

-- | This is provided with vector-0.12, but we include a copy here 
--   for compatibility.
vectorConcatNE :: NonEmpty (Vector a) -> Vector a
vectorConcatNE = Vector.concat . toList

