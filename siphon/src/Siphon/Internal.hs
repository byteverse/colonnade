{-# LANGUAGE BangPatterns #-}

-- | A CSV parser. The parser defined here is RFC 4180 compliant, with
-- the following extensions:
--
--  * Empty lines are ignored.
--
--  * Non-escaped fields may contain any characters except
--    double-quotes, commas, carriage returns, and newlines.
--
--  * Escaped fields may contain any characters (but double-quotes
--    need to be escaped).
--
-- The functions in this module can be used to implement e.g. a
-- resumable parser that is fed input incrementally.
module Siphon.Internal where

import Siphon.Types

import Data.ByteString.Builder (toLazyByteString,byteString)
import qualified Data.ByteString.Char8 as BC8
import Control.Applicative (optional)
import Data.Attoparsec.ByteString.Char8 (char, endOfInput, string)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Lazy as AL
import qualified Data.Attoparsec.Zepto as Z
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString.Builder as Builder
import Data.Word (Word8)
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Siphon.Types

import Control.Applicative
import Data.Monoid

byteStringChar8 :: Siphon ByteString
byteStringChar8 = Siphon 
  escape 
  encodeRow
  (A.parse (row comma))
  B.null

encodeRow :: Vector (Escaped ByteString) -> ByteString
encodeRow = id
  . flip B.append (B.singleton newline)
  . B.intercalate (B.singleton comma)
  . V.toList
  . coerce

escape :: ByteString -> Escaped ByteString
escape t = case B.find (\c -> c == newline || c == comma || c == doubleQuote) t of
  Nothing -> Escaped t
  Just _  -> escapeAlways t

-- | This implementation is definitely suboptimal.
-- A better option (which would waste a little space
-- but would be much faster) would be to build the
-- new bytestring by writing to a buffer directly.
escapeAlways :: ByteString -> Escaped ByteString
escapeAlways t = Escaped $ LByteString.toStrict $ Builder.toLazyByteString $
     Builder.word8 doubleQuote
  <> B.foldl
      (\ acc b -> acc <> if b == doubleQuote
          then Builder.byteString
            (B.pack [doubleQuote,doubleQuote])
          else Builder.word8 b)
      mempty
      t
  <> Builder.word8 doubleQuote

-- | Specialized version of 'sepBy1'' which is faster due to not
-- accepting an arbitrary separator.
sepByDelim1' :: AL.Parser a
             -> Word8  -- ^ Field delimiter
             -> AL.Parser [a]
sepByDelim1' p !delim = liftM2' (:) p loop
  where
    loop = do
        mb <- A.peekWord8
        case mb of
            Just b | b == delim -> liftM2' (:) (A.anyWord8 *> p) loop
            _                   -> pure []
{-# INLINE sepByDelim1' #-}

-- | Specialized version of 'sepBy1'' which is faster due to not
-- accepting an arbitrary separator.
sepByEndOfLine1' :: AL.Parser a
                 -> AL.Parser [a]
sepByEndOfLine1' p = liftM2' (:) p loop
  where
    loop = do
        mb <- A.peekWord8
        case mb of
            Just b | b == cr ->
                liftM2' (:) (A.anyWord8 *> A.word8 newline *> p) loop
                   | b == newline ->
                liftM2' (:) (A.anyWord8 *> p) loop
            _ -> pure []
{-# INLINE sepByEndOfLine1' #-}

-- | Parse a record, not including the terminating line separator. The
-- terminating line separate is not included as the last record in a
-- CSV file is allowed to not have a terminating line separator. You
-- most likely want to use the 'endOfLine' parser in combination with
-- this parser.
row :: Word8  -- ^ Field delimiter
    -> AL.Parser (Vector ByteString)
row !delim = rowNoNewline delim <* endOfLine
{-# INLINE row #-}

rowNoNewline :: Word8  -- ^ Field delimiter
             -> AL.Parser (Vector ByteString)
rowNoNewline !delim = V.fromList <$!> field delim `sepByDelim1'` delim
{-# INLINE rowNoNewline #-}

removeBlankLines :: [Vector ByteString] -> [Vector ByteString]
removeBlankLines = filter (not . blankLine)

-- | Parse a field. The field may be in either the escaped or
-- non-escaped format. The return value is unescaped.
field :: Word8 -> AL.Parser ByteString
field !delim = do
    mb <- A.peekWord8
    -- We purposely don't use <|> as we want to commit to the first
    -- choice if we see a double quote.
    case mb of
        Just b | b == doubleQuote -> escapedField
        _                         -> unescapedField delim
{-# INLINE field #-}

escapedField :: AL.Parser S.ByteString
escapedField = do
    _ <- dquote
    -- The scan state is 'True' if the previous character was a double
    -- quote.  We need to drop a trailing double quote left by scan.
    s <- S.init <$> (A.scan False $ \s c -> if c == doubleQuote
                                            then Just (not s)
                                            else if s then Nothing
                                                 else Just False)
    if doubleQuote `S.elem` s
        then case Z.parse unescape s of
            Right r  -> return r
            Left err -> fail err
        else return s

unescapedField :: Word8 -> AL.Parser S.ByteString
unescapedField !delim = A.takeWhile (\ c -> c /= doubleQuote &&
                                            c /= newline &&
                                            c /= delim &&
                                            c /= cr)

dquote :: AL.Parser Char
dquote = char '"'

-- | This could be improved. We could avoid the builder and just
-- write to a buffer directly.
unescape :: Z.Parser S.ByteString
unescape = (LByteString.toStrict . toLazyByteString) <$!> go mempty where
  go acc = do
    h <- Z.takeWhile (/= doubleQuote)
    let rest = do
          start <- Z.take 2
          if (S.unsafeHead start == doubleQuote &&
              S.unsafeIndex start 1 == doubleQuote)
              then go (acc `mappend` byteString h `mappend` byteString (BC8.singleton '"'))
              else fail "invalid CSV escape sequence"
    done <- Z.atEnd
    if done
      then return (acc `mappend` byteString h)
      else rest

-- | A strict version of 'Data.Functor.<$>' for monads.
(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
    a <- m
    return $! f a
{-# INLINE (<$!>) #-}

infixl 4 <$!>

-- | Is this an empty record (i.e. a blank line)?
blankLine :: V.Vector B.ByteString -> Bool
blankLine v = V.length v == 1 && (B.null (V.head v))

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
endOfLine = (A.word8 newline *> return ()) <|> (string (BC8.pack "\r\n") *> return ()) <|> (A.word8 cr *> return ())
{-# INLINE endOfLine #-}

doubleQuote, newline, cr, comma :: Word8
doubleQuote = 34
newline = 10
cr = 13
comma = 44

