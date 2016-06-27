{-# LANGUAGE RankNTypes #-}

module Siphon.Decoding where

import Siphon.Types
import Siphon.Internal (row,comma)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Pipes (yield,Pipe,Consumer',Producer,await)
import Data.Vector (Vector)
import qualified Data.Attoparsec.ByteString as AttoByteString
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Attoparsec.Types as Atto

byteStringChar8 :: SiphonDecoding ByteString ByteString
byteStringChar8 = SiphonDecoding
  (AttoByteString.parse (row comma))
  ByteString.null

-- unrow :: c1 -> (Vector c2,c1)
-- 
-- row :: _
--     -> Decoding (Indexed f) c a
--     -> Vector c
--     -> Either DecodingErrors a

pipe :: Monad m
     => SiphonDecoding c1 c2 
     -> Atto.Parser c1 (WithEnd c2) 
     -> Pipe c1 (Vector c2) m String
pipe (SiphonDecoding parse isNull) p = go1 where
  go1 = do
    c1 <- awaitSkip isNull
    handleResult (parse c1)
  go2 c1 = handleResult (parse c1)
  go3 k = do
    c1 <- awaitSkip isNull
    handleResult (k c1)
  handleResult r = case r of
    Atto.Fail _ _ _ -> error "ahh"
    Atto.Done c1 v -> do
      yield v
      if isNull c1 then go1 else go2 c1
    Atto.Partial k -> go3 k

awaitSkip :: Monad m
          => (a -> Bool)
          -> Consumer' a m a
awaitSkip f = go where
  go = do
    a <- await
    if f a then go else return a 


