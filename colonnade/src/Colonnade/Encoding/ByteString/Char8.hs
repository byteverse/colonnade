module Colonnade.Encoding.ByteString.Char8 where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Builder    as Builder
import qualified Data.ByteString.Lazy       as LByteString

char :: Char -> ByteString
char = BC8.singleton

int :: Int -> ByteString
int = LByteString.toStrict
    . Builder.toLazyByteString 
    . Builder.intDec

bool :: Bool -> ByteString
bool x = case x of
  True -> BC8.pack "true"
  False -> BC8.pack "false"

byteString :: ByteString -> ByteString
byteString = id

