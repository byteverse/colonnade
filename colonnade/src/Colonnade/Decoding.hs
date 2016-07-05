{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns        #-}
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

indexed :: Int -> (content -> Either String a) -> Decoding (Indexed Headless) content a
indexed ix f = DecodingAp (Indexed ix Headless) f (DecodingPure id)

maxIndex :: forall f c a. Decoding (Indexed f) c a -> Int
maxIndex = go 0 where
  go :: forall b. Int -> Decoding (Indexed f) c b -> Int
  go !ix (DecodingPure _) = ix
  go !ix1 (DecodingAp (Indexed ix2 _) decode apNext) =
    go (max ix1 ix2) apNext

-- | This function uses 'unsafeIndex' to access
--   elements of the 'Vector'.
uncheckedRunWithRow ::
     Int
  -> Decoding (Indexed f) content a
  -> Vector content
  -> Either (DecodingRowError f content) a
uncheckedRunWithRow i d v = mapLeft (DecodingRowError i . RowErrorDecode) (uncheckedRun d v)

-- | This function does not check to make sure that the indicies in
--   the 'Decoding' are in the 'Vector'.
uncheckedRun :: forall content a f.
                Decoding (Indexed f) content a
             -> Vector content
             -> Either (DecodingCellErrors f content) a
uncheckedRun dc v = getEitherWrap (go dc)
  where
  go :: forall b.
        Decoding (Indexed f) content b
     -> EitherWrap (DecodingCellErrors f content) b
  go (DecodingPure b) = EitherWrap (Right b)
  go (DecodingAp ixed@(Indexed ix h) decode apNext) =
    let rnext = go apNext
        content = Vector.unsafeIndex v ix
        rcurrent = mapLeft (DecodingCellErrors . Vector.singleton . DecodingCellError content ixed) (decode content)
    in rnext <*> (EitherWrap rcurrent)

headlessToIndexed :: forall c a.
  Decoding Headless c a -> Decoding (Indexed Headless) c a
headlessToIndexed = go 0 where
  go :: forall b. Int -> Decoding Headless c b -> Decoding (Indexed Headless) c b
  go !ix (DecodingPure a) = DecodingPure a
  go !ix (DecodingAp Headless decode apNext) =
    DecodingAp (Indexed ix Headless) decode (go (ix + 1) apNext)

length :: forall f c a. Decoding f c a -> Int
length = go 0 where
  go :: forall b. Int -> Decoding f c b -> Int
  go !a (DecodingPure _) = a
  go !a (DecodingAp _ _ apNext) = go (a + 1) apNext

-- | Maps over a 'Decoding' that expects headers, converting these
--   expected headers into the indices of the columns that they
--   correspond to.
headedToIndexed :: forall content a. Eq content
                => Vector content -- ^ Headers in the source document
                -> Decoding Headed content a -- ^ Decoding that contains expected headers
                -> Either (HeadingErrors content) (Decoding (Indexed Headed) content a)
headedToIndexed v = getEitherWrap . go
  where
  go :: forall b. Eq content
     => Decoding Headed content b
     -> EitherWrap (HeadingErrors content) (Decoding (Indexed Headed) content b)
  go (DecodingPure b) = EitherWrap (Right (DecodingPure b))
  go (DecodingAp hd@(Headed h) decode apNext) =
    let rnext = go apNext
        ixs = Vector.elemIndices h v
        ixsLen = Vector.length ixs
        rcurrent
          | ixsLen == 1 = Right (Vector.unsafeIndex ixs 0)
          | ixsLen == 0 = Left (HeadingErrors (Vector.singleton h) Vector.empty)
          | otherwise   = Left (HeadingErrors Vector.empty (Vector.singleton (h,ixsLen)))
    in (\ix ap -> DecodingAp (Indexed ix hd) decode ap)
       <$> EitherWrap rcurrent
       <*> rnext

eitherMonoidAp :: Monoid a => Either a (b -> c) -> Either a b -> Either a c
eitherMonoidAp = go where
  go (Left a1) (Left a2) = Left (mappend a1 a2)
  go (Left a1) (Right _) = Left a1
  go (Right _) (Left a2) = Left a2
  go (Right f) (Right b) = Right (f b)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft _ (Right a) = Right a
mapLeft f (Left a) = Left (f a)

