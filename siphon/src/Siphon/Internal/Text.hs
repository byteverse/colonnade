{-# LANGUAGE BangPatterns #-}

module Siphon.Internal.Text where

import Siphon.Types

import Control.Applicative (optional)
import Data.Attoparsec.Text (char, endOfInput, string)
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Text.Lazy as AL
import qualified Data.Attoparsec.Zepto as Z
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as Builder
import Data.Text.Lazy.Builder (Builder)
import Data.Word (Word8)
import Data.Vector (Vector)
import Data.Text (Text)
import Data.Coerce (coerce)
import Siphon.Types

import Control.Applicative
import Data.Monoid

text :: Siphon Text
text = Siphon
  escape
  encodeRow
  (A.parse (row comma))
  Text.null

encodeRow :: Vector (Escaped Text) -> Text
encodeRow = id
  . flip Text.append (Text.singleton newline)
  . Text.intercalate (Text.singleton comma)
  . V.toList
  . coerce

escape :: Text -> Escaped Text
escape t = case Text.find (\c -> c == newline || c == cr || c == comma || c == doubleQuote) t of
  Nothing -> Escaped t
  Just _  -> escapeAlways t

-- | This implementation is definitely suboptimal.
-- A better option (which would waste a little space
-- but would be much faster) would be to build the
-- new text by writing to a buffer directly.
escapeAlways :: Text -> Escaped Text
escapeAlways t = Escaped $ Text.concat
  [ textDoubleQuote
  , Text.replace textDoubleQuote (Text.pack [doubleQuote,doubleQuote]) t
  , textDoubleQuote
  ]

-- | Specialized version of 'sepBy1'' which is faster due to not
-- accepting an arbitrary separator.
sepByDelim1' :: A.Parser a
             -> Char  -- ^ Field delimiter
             -> A.Parser [a]
sepByDelim1' p !delim = liftM2' (:) p loop
  where
    loop = do
        mb <- A.peekChar
        case mb of
            Just b | b == delim -> liftM2' (:) (A.anyChar *> p) loop
            _                   -> pure []
{-# INLINE sepByDelim1' #-}

-- | Specialized version of 'sepBy1'' which is faster due to not
-- accepting an arbitrary separator.
sepByEndOfLine1' :: A.Parser a
                 -> A.Parser [a]
sepByEndOfLine1' p = liftM2' (:) p loop
  where
    loop = do
        mb <- A.peekChar
        case mb of
            Just b | b == cr ->
                liftM2' (:) (A.anyChar *> A.char newline *> p) loop
                   | b == newline ->
                liftM2' (:) (A.anyChar *> p) loop
            _ -> pure []
{-# INLINE sepByEndOfLine1' #-}

-- | Parse a record, not including the terminating line separator. The
-- terminating line separate is not included as the last record in a
-- CSV file is allowed to not have a terminating line separator. You
-- most likely want to use the 'endOfLine' parser in combination with
-- this parser.
row :: Char  -- ^ Field delimiter
    -> A.Parser (Vector Text)
row !delim = rowNoNewline delim <* endOfLine
{-# INLINE row #-}

rowNoNewline :: Char  -- ^ Field delimiter
             -> A.Parser (Vector Text)
rowNoNewline !delim = V.fromList <$!> field delim `sepByDelim1'` delim
{-# INLINE rowNoNewline #-}

-- | Parse a field. The field may be in either the escaped or
-- non-escaped format. The return value is unescaped.
field :: Char -> A.Parser Text
field !delim = do
    mb <- A.peekChar
    -- We purposely don't use <|> as we want to commit to the first
    -- choice if we see a double quote.
    case mb of
        Just b | b == doubleQuote -> escapedField
        _                         -> unescapedField delim
{-# INLINE field #-}

escapedField :: A.Parser Text
escapedField = do
  _ <- dquote -- This can probably be replaced with anyChar
  b <- escapedFieldInner mempty
  return (LText.toStrict (Builder.toLazyText b))

escapedFieldInner :: Builder -> A.Parser Builder
escapedFieldInner b = do
  t <- A.takeTill (== doubleQuote)
  _ <- A.anyChar -- this will always be a double quote
  c <- A.peekChar'
  if c == doubleQuote
    then do
      _ <- A.anyChar -- this will always be a double quote
      escapedFieldInner (b `mappend` Builder.fromText t `mappend` Builder.fromText textDoubleQuote)
    else return (b `mappend` Builder.fromText t)

unescapedField :: Char -> A.Parser Text
unescapedField !delim = A.takeWhile (\ c -> c /= doubleQuote &&
                                            c /= newline &&
                                            c /= delim &&
                                            c /= cr)

dquote :: A.Parser Char
dquote = char doubleQuote

unescape :: A.Parser Text
unescape = (LText.toStrict . Builder.toLazyText) <$!> go mempty where
  go acc = do
    h <- A.takeWhile (/= doubleQuote)
    let rest = do
          c0 <- A.anyChar
          c1 <- A.anyChar
          if (c0 == doubleQuote && c1 == doubleQuote)
              then go (acc `mappend` Builder.fromText h `mappend` Builder.fromText textDoubleQuote)
              else fail "invalid CSV escape sequence"
    done <- A.atEnd
    if done
      then return (acc `mappend` Builder.fromText h)
      else rest

-- | A strict version of 'Data.Functor.<$>' for monads.
(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
    a <- m
    return $! f a
{-# INLINE (<$!>) #-}

infixl 4 <$!>

-- | A version of 'liftM2' that is strict in the result of its first
-- action.
liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
    !x <- a
    y <- b
    return (f x y)
{-# INLINE liftM2' #-}


-- | Match either a single newline character @\'\\n\'@, or a carriage
-- return followed by a newline character @\"\\r\\n\"@, or a single
-- carriage return @\'\\r\'@.
endOfLine :: A.Parser ()
endOfLine = (A.char newline *> return ()) <|> (string (Text.pack "\r\n") *> return ()) <|> (A.char cr *> return ())
{-# INLINE endOfLine #-}

textDoubleQuote :: Text
textDoubleQuote = Text.singleton doubleQuote

doubleQuote, newline, cr, comma :: Char
doubleQuote = '\"'
newline = '\n'
cr = '\r'
comma = ','

