module Colonnade.Encoding.Text where

import Data.Text
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

char :: Char -> Text
char = Text.singleton

int :: Int -> Text
int = LText.toStrict
    . Builder.toLazyText
    . Builder.decimal

text :: Text -> Text
text = id

