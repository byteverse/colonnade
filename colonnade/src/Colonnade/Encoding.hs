module Colonnade.Encoding where

import Colonnade.Types
import qualified Data.Vector as Vector

mapContent :: Functor f => (c1 -> c2) -> Encoding f c1 a -> Encoding f c2 a
mapContent f (Encoding v) = Encoding
  $ Vector.map (\(h,c) -> (fmap f h,f . c)) v

headless :: (a -> content) -> Encoding Headless content a
headless f = Encoding (Vector.singleton (Headless,f))

headed :: content -> (a -> content) -> Encoding Headed content a
headed h f = Encoding (Vector.singleton (Headed h,f))

