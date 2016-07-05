module Colonnade.Encoding where

import Colonnade.Types
import Data.Vector (Vector)
import qualified Data.Vector as Vector

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

runRowMonadic :: Monad m
              => Encoding f content a
              -> (content -> m ())
              -> a
              -> m ()
runRowMonadic (Encoding v) g a = Vector.forM_ v $ \e ->
  g (oneEncodingEncode e a)

runHeader :: (c1 -> c2) -> Encoding Headed c1 a -> Vector c2
runHeader g (Encoding v) =
  Vector.map (g . getHeaded . oneEncodingHead) v

runHeaderMonadic :: Monad m
                 => Encoding Headed content a
                 -> (content -> m ())
                 -> m ()
runHeaderMonadic (Encoding v) g =
  Vector.mapM_ (g . getHeaded . oneEncodingHead) v



