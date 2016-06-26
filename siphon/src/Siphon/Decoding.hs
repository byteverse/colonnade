module Siphon.Decoding where

import Siphon.Types
import qualified Data.Attoparsec.Types as Atto

-- unrow :: c1 -> (Vector c2,c1)
-- 
-- row :: _
--     -> Decoding (Indexed f) c a
--     -> Vector c
--     -> Either DecodingErrors a

pipe :: SiphonDecoding c1 c2 
     -> Atto.Parser c1 (WithEnd c2) 
     -> Pipe c1 (Vector c2) m String
pipe (SiphonDecoding parse isNull) p = do
  c1 <- awaitSkip isNull
  case parse p c1 of

awaitSkip :: (a -> Bool)
          -> Consumer' a m a
awaitSkip f = go where
  go = do
    a <- await
    if f a then go else return a 

nextSkipEmpty
  :: (Monad m, Eq a, Monoid a)
  => Producer a m r
  -> m (Either r (a, Producer a m r))
nextSkipEmpty = go where
    go p0 = do
      x <- next p0
      case x of
         Left  _        -> return x
         Right (a,p1)
          | a == mempty -> go p1
          | otherwise   -> return x
{-# INLINABLE nextSkipEmpty #-}


