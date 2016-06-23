{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Colonnade.Decoding where

import Colonnade.Internal (EitherWrap(..))
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

uncheckedRun :: forall content a.
                Vector content
             -> Decoding Indexed content a
             -> Either (DecodingErrors Indexed content) a
uncheckedRun v = getEitherWrap . go
  where
  go :: forall b. 
        Decoding Indexed content b
     -> EitherWrap (DecodingErrors Indexed content) b
  go (DecodingPure b) = EitherWrap (Right b)
  go (DecodingAp (Indexed ix) decode apNext) =
    let rnext = go apNext
        content = Vector.unsafeIndex v ix
        rcurrent = mapLeft (DecodingErrors . Vector.singleton . DecodingError content (Indexed ix)) (decode content)
    in rnext <*> (EitherWrap rcurrent)

-- | Maps over a 'Decoding' that expects headers, converting these
--   expected headers into the indices of the columns that they
--   correspond to.
headedToIndexed :: forall content a. Eq content
                => Vector content -- ^ Headers in the source document
                -> Decoding Headed content a -- ^ Decoding that contains expected headers
                -> Either (HeadingErrors content) (Decoding Indexed content a)
headedToIndexed v = go
  where
  go :: forall b. Eq content
     => Decoding Headed content b
     -> Either (HeadingErrors content) (Decoding Indexed content b)
  go (DecodingPure b) = Right (DecodingPure b)
  go (DecodingAp (Headed h) decode apNext) =
    let rnext = go apNext
        ixs = Vector.elemIndices h v
        ixsLen = Vector.length ixs
        rcurrent
          | ixsLen == 1 = Right (Vector.unsafeIndex ixs 0)
          | ixsLen == 0 = Left (HeadingErrors (Vector.singleton h) Vector.empty)
          | otherwise   = Left (HeadingErrors Vector.empty (Vector.singleton (h,ixsLen)))
    in case rcurrent of
         Right ix -> case rnext of
           Right apIx -> Right (DecodingAp (Indexed ix) decode apIx)
           Left errNext -> Left errNext
         Left err -> case rnext of
           Right _ -> Left err
           Left errNext -> Left (mappend err errNext)

eitherMonoidAp :: Monoid a => Either a (b -> c) -> Either a b -> Either a c
eitherMonoidAp = go where
  go (Left a1) (Left a2) = Left (mappend a1 a2)
  go (Left a1) (Right _) = Left a1
  go (Right _) (Left a2) = Left a2
  go (Right f) (Right b) = Right (f b)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft _ (Right a) = Right a
mapLeft f (Left a) = Left (f a)

