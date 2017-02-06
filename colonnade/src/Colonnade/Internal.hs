{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_HADDOCK not-home #-}

module Colonnade.Internal
  ( Colonnade(..)
  , OneColonnade(..)
  , Headed(..)
  , Headless(..)
  ) where

import Data.Vector (Vector)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Functor.Contravariant.Divisible (Divisible(..))
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import qualified Data.Vector as Vector

-- | As the first argument to the 'Colonnade' type 
--   constructor, this indictates that the columnar encoding has 
--   a header. This type is isomorphic to 'Identity' but is 
--   given a new name to clarify its intent:
--
-- > example :: Colonnade Headed Text Foo
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
-- > example :: Colonnade Headless Text Foo
--
--   The term @example@ represents a columnar encoding of @Foo@
--   in which the columns do not have headings.
data Headless a = Headless
  deriving (Eq,Ord,Functor,Show,Read,Foldable)

instance Contravariant Headless where
  contramap _ Headless = Headless

-- | Encodes a header and a cell.
data OneColonnade h content a = OneColonnade
  { oneColonnadeHead   :: !(h content)
  , oneColonnadeEncode :: !(a -> content)
  }

instance Contravariant (OneColonnade h content) where
  contramap f (OneColonnade h e) = OneColonnade h (e . f)

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
-- >             +---- Content (Text, ByteString, Html, etc.)
-- >             |
-- >             v
-- > Colonnade h c a
-- >           ^   ^
-- >           |   |
-- >           |   +-- Value consumed to build a row
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
newtype Colonnade h c a = Colonnade
  { getColonnade :: Vector (OneColonnade h c a)
  } deriving (Monoid)

instance Contravariant (Colonnade h content) where
  contramap f (Colonnade v) = Colonnade
    (Vector.map (contramap f) v)

instance Divisible (Colonnade h content) where
  conquer = Colonnade Vector.empty
  divide f (Colonnade a) (Colonnade b) =
    Colonnade $ (Vector.++)
      (Vector.map (contramap (fst . f)) a)
      (Vector.map (contramap (snd . f)) b)

