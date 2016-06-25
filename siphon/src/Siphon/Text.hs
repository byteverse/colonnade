module Siphon.Text where

import Siphon.Types
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Coerce (coerce)
import qualified Data.Text   as Text
import qualified Data.Vector as Vector

siphon :: Siphon Text Text
siphon = Siphon escape encodeRow

encodeRow :: Vector (Escaped Text) -> Text
encodeRow = id
  . Text.intercalate (Text.pack ",")
  . Vector.toList
  . coerce

escape :: Text -> Escaped Text
escape t = case Text.find (\c -> c == '\n' || c == ',' || c == '"') t of
  Nothing -> Escaped t
  Just _  -> escapeAlways t

escapeAlways :: Text -> Escaped Text
escapeAlways t = Escaped $ Text.concat
  [ Text.singleton '"'
  , Text.replace (Text.pack "\"") (Text.pack "\"\"") t
  , Text.singleton '"'
  ]



