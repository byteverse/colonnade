{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Colonnade.Decoding where

import Colonnade.Types
import Data.Functor.Contravariant
import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- | Converts the content type of a 'Decoding'. The @'Contravariant' f@
-- constraint means that @f@ can be 'Headless' but not 'Headed'.
contramapContent :: forall c1 c2 f a. Contravariant f => (c2 -> c1) -> Decoding f c1 a -> Decoding f c2 a
contramapContent f = go
  where
  go :: forall b. Decoding f c1 b -> Decoding f c2 b
  go (DecodingPure x) = DecodingPure x
  go (DecodingAp h decode apNext) =
    DecodingAp (contramap f h) (decode . f) (go apNext)

headless :: (content -> Either String a) -> Decoding Headless content a
headless f = DecodingAp Headless f (DecodingPure id)

headed :: content -> (content -> Either String a) -> Decoding Headed content a
headed h f = DecodingAp (Headed h) f (DecodingPure id)

-- | Maps over a 'Decoding' that expects headers, converting these
--   expected headers into the indices of the columns that they
--   correspond to.
headedToIndexed :: forall content a. Eq content
                => Vector content -- ^ Headers in the source document
                -> Decoding Headed content a -- ^ Decoding that contains expected headers
                -> Either (HeadingError content) (Decoding Indexed content a)
headedToIndexed v = go
  where
  go :: forall b. Eq content
     => Decoding Headed content b
     -> Either (HeadingError content) (Decoding Indexed content b)
  go (DecodingPure b) = Right (DecodingPure b)
  go (DecodingAp (Headed h) decode apNext) =
    let rnext = go apNext
        ixs = Vector.elemIndices h v
        ixsLen = Vector.length ixs
        rcurrent
          | ixsLen == 1 = Right (Vector.unsafeIndex ixs 0)
          | ixsLen == 0 = Left (HeadingError (Vector.singleton h) Vector.empty)
          | otherwise   = Left (HeadingError Vector.empty (Vector.singleton (h,ixsLen)))
    in case rcurrent of
         Right ix -> case rnext of
           Right apIx -> Right (DecodingAp (Indexed ix) decode apIx)
           Left errNext -> Left errNext
         Left err -> case rnext of
           Right _ -> Left err
           Left errNext -> Left (mappend err errNext)


