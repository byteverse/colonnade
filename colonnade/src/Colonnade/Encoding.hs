module Colonnade.Encoding where

import Colonnade.Types
import Data.Vector (Vector)
import Data.Foldable
import qualified Data.Vector as Vector
import qualified Colonnade.Internal as Internal

mapContent :: Functor f => (c1 -> c2) -> Encoding f c1 a -> Encoding f c2 a
mapContent f (Encoding v) = Encoding
  $ Vector.map (\(OneEncoding h c) -> (OneEncoding (fmap f h) (f . c))) v

headless :: (a -> content) -> Encoding Headless content a
headless f = Encoding (Vector.singleton (OneEncoding Headless f))

headed :: content -> (a -> content) -> Encoding Headed content a
headed h f = Encoding (Vector.singleton (OneEncoding (Headed h) f))

-- runRow' :: Encoding f content a -> a -> Vector content
-- runRow' = runRow id

-- | Consider providing a variant the produces a list
-- instead. It may allow more things to get inlined
-- in to a loop.
runRow :: (c1 -> c2) -> Encoding f c1 a -> a -> Vector c2
runRow g (Encoding v) a = flip Vector.map v $
  \(OneEncoding _ encode) -> g (encode a)

runRowMonadic :: (Monad m, Monoid b)
              => Encoding f content a
              -> (content -> m b)
              -> a
              -> m b
runRowMonadic (Encoding v) g a =
  flip Internal.foldMapM v
  $ \e -> g (oneEncodingEncode e a)

runRowMonadicWith :: (Monad m)
              => b
              -> (b -> b -> b)
              -> Encoding f content a
              -> (content -> m b)
              -> a
              -> m b
runRowMonadicWith bempty bappend (Encoding v) g a =
  foldrM (\e br -> do
    bl <- g (oneEncodingEncode e a)
    return (bappend bl br)
  ) bempty v

runHeader :: (c1 -> c2) -> Encoding Headed c1 a -> Vector c2
runHeader g (Encoding v) =
  Vector.map (g . getHeaded . oneEncodingHead) v

runHeaderMonadic :: (Monad m, Monoid b)
                 => Encoding Headed content a
                 -> (content -> m b)
                 -> m b
runHeaderMonadic (Encoding v) g =
  fmap (mconcat . Vector.toList) $ Vector.mapM (g . getHeaded . oneEncodingHead) v

runHeaderMonadic_ ::
     (Monad m)
  => Encoding Headed content a
  -> (content -> m b)
  -> m ()
runHeaderMonadic_ (Encoding v) g = Vector.mapM_ (g . getHeaded . oneEncodingHead) v

fromMaybe :: c -> Encoding f c a -> Encoding f c (Maybe a)
fromMaybe c (Encoding v) = Encoding $ flip Vector.map v $
  \(OneEncoding h encode) -> OneEncoding h (maybe c encode)

columns :: (b -> a -> c)
        -> (b -> f c)
        -> Vector b
        -> Encoding f c a
columns getCell getHeader bs =
  Encoding $ Vector.map (\b -> OneEncoding (getHeader b) (getCell b)) bs



