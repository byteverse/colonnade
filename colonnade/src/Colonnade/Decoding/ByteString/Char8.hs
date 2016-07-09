module Colonnade.Decoding.ByteString.Char8 where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BC8

char :: ByteString -> Either String Char
char b = case BC8.length b of
  1 -> Right (BC8.head b)
  0 -> Left "cannot decode Char from empty bytestring"
  _ -> Left "cannot decode Char from multi-character bytestring"

int :: ByteString -> Either String Int
int b = do
  (a,bsRem) <- maybe (Left "could not parse int") Right (BC8.readInt b)
  if ByteString.null bsRem
    then Right a
    else Left "found extra characters after int"

bool :: ByteString -> Either String Bool
bool b
  | b == BC8.pack "true" = Right True
  | b == BC8.pack "false" = Right False
  | otherwise = Left "must be true or false"


