module Colonnade.Decoding.Text where

import Prelude hiding (map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead

char :: Text -> Either String Char
char t = case Text.length t of
  1 -> Right (Text.head t)
  0 -> Left "cannot decode Char from empty text"
  _ -> Left "cannot decode Char from multi-character text"

text :: Text -> Either String Text
text = Right

int :: Text -> Either String Int
int t = do
  (a,tRem) <- TextRead.decimal t
  if Text.null tRem
    then Right a
    else Left "found extra characters after int"

trueFalse :: Text -> Text -> Text -> Either String Bool
trueFalse t f txt
  | txt == t = Right True
  | txt == f = Right False
  | otherwise = Left $ concat
      ["must be [", Text.unpack t, "] or [", Text.unpack f, "]"]

-- | This refers to the 'TextRead.Reader' from @Data.Text.Read@, not
--   to the @Reader@ monad.
fromReader :: TextRead.Reader a -> Text -> Either String a
fromReader f t = do
  (a,tRem) <- f t
  if Text.null tRem
    then Right a
    else Left "found extra characters at end of text"

optional :: (Text -> Either String a) -> Text -> Either String (Maybe a)
optional f t = if Text.null t
  then Right Nothing
  else fmap Just (f t)

map :: (a -> b) -> (Text -> Either String a) -> Text -> Either String b
map f g t = fmap f (g t)

