{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Colonnade.Decoding where

import Colonnade.Types
import Data.Functor.Contravariant

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


