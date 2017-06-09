{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

-- {-# OPTIONS_GHC -Wall -Werr -fno-warn-unused-imports #-}

module Siphon
  ( Siphon
  , SiphonError
  , Indexed(..)
  , decodeHeadedChar8Csv
  , humanizeSiphonError
  ) where

import Siphon.Types
import Data.Monoid
import Control.Applicative
import Control.Monad

import qualified Data.ByteString.Char8 as BC8
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Lazy as AL
import qualified Data.Attoparsec.Zepto as Z
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as T
import qualified Data.List as L
import qualified Streaming as SM
import qualified Streaming.Prelude as SMP
import qualified Data.Attoparsec.Types as ATYP
import qualified Colonnade.Encode as CE

import Control.Monad.Trans.Class
import Data.ByteString.Builder (toLazyByteString,byteString)
import Data.Attoparsec.ByteString.Char8 (char, endOfInput, string)
import Data.Word (Word8)
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Char (chr)
import Streaming (Stream,Of(..))

newtype Escaped c = Escaped { getEscaped :: c }
data Ended = EndedYes | EndedNo
data CellResult c = CellResultData !c | CellResultNewline !Ended

decodeHeadedChar8Csv :: Monad m 
  => Siphon CE.Headed ByteString a
  -> Stream (Of ByteString) m () -- ^ encoded csv
  -> Stream (Of a) m (Maybe (SiphonError ByteString))
decodeHeadedChar8Csv headedSiphon s1 = do
  e <- lift (consumeHeaderRowChar8 s1)
  case e of
    Left err -> return (Just err)
    Right (v :> s2) -> case headedToIndexed v headedSiphon of
      Left err -> return (Just err)
      Right ixedSiphon -> do
        let requiredLength = V.length v
        consumeBodyChar8 1 requiredLength ixedSiphon s2

encodeHeadedChar8Csv :: Monad m 
  => Colonnade CE.Headed ByteString a
  -> Stream (Of a) m r
  -> Stream (Of ByteString) m r
encodeHeadedChar8Csv headedSiphon s1 = do
  yield (header siphon encoding)
  pipe siphon encoding

encodeGeneralCsv :: Monad m
  => (c -> Escaped c)
  -> c -- ^ separator
  -> Colonnade f a c
  -> Stream (Of a) m r
  -> Stream (Of c) m r
encodeGeneralCsv escapeFunc separatorStr colonnade = do
  Pipes.map (row siphon encoding)

encodeHeader :: Siphon c -> Colonnade Headed a c -> c
  => (c -> Escaped c)
  -> c -- ^ separator
  -> Colonnade f a c
  -> Stream (Of c) m r
encodeHeader escapeFunc separatorStr colonnade = SMP.mapM_ $ \a -> do
  let (vs,ws) = V.splitAt 1 (CE.getColonnade colonnade)
  -- we only need to do this split because the first cell
  -- gets treated differently than the others. It does not
  -- get a separator added before it.
  V.forM_ vs $ \(CE.OneColonnade _ encode) -> yield (getEscaped (escapeFunc (encode a)))
  V.forM_ ws $ \(CE.OneColonnade _ encode) -> do
    yield separator
    yeied (getEscaped (escapeFunc (encode a)))

encodeRow :: 
  => (c -> Escaped c)
  -> c -- ^ separator
  -> Colonnade f a c
  -> Stream (Of a) m r
  -> Stream (Of c) m r
encodeRow escapeFunc separatorStr colonnade = SMP.mapM_ $ \a -> do
  let (vs,ws) = V.splitAt 1 (CE.getColonnade colonnade)
  -- we only need to do this split because the first cell
  -- gets treated differently than the others. It does not
  -- get a separator added before it.
  V.forM_ vs $ \(CE.OneColonnade _ encode) -> yield (getEscaped (escapeFunc (encode a)))
  V.forM_ ws $ \(CE.OneColonnade _ encode) -> do
    yield separator
    yeied (getEscaped (escapeFunc (encode a)))

data IndexedHeader a = IndexedHeader
  { indexedHeaderIndexed :: {-# UNPACK #-} !Int
  , indexedHeaderHeader :: !a
  } 

-- | Maps over a 'Decolonnade' that expects headers, converting these
--   expected headers into the indices of the columns that they
--   correspond to.
headedToIndexed :: forall c a. Eq c
  => Vector c -- ^ Headers in the source document
  -> Siphon CE.Headed c a -- ^ Decolonnade that contains expected headers
  -> Either (SiphonError c) (Siphon IndexedHeader c a)
headedToIndexed v =
    mapLeft (\(HeaderErrors a b c) -> SiphonError 0 (RowErrorHeaders a b c)) 
  . getEitherWrap
  . go
  where
  go :: forall b.
        Siphon CE.Headed c b
     -> EitherWrap (HeaderErrors c) (Siphon IndexedHeader c b)
  go (SiphonPure b) = EitherWrap (Right (SiphonPure b))
  go (SiphonAp (CE.Headed h) decode apNext) =
    let rnext = go apNext
        ixs = V.elemIndices h v
        ixsLen = V.length ixs
        rcurrent
          | ixsLen == 1 = Right (V.unsafeIndex ixs 0)
          | ixsLen == 0 = Left (HeaderErrors V.empty (V.singleton h) V.empty)
          | otherwise   =
              let dups = V.singleton (V.map (\ix -> CellError ix (V.unsafeIndex v ix)) ixs)
               in Left (HeaderErrors dups V.empty V.empty)
    in (\ix nextSiphon -> SiphonAp (IndexedHeader ix h) decode nextSiphon)
       <$> EitherWrap rcurrent
       <*> rnext

data HeaderErrors c = HeaderErrors !(Vector (Vector (CellError c))) !(Vector c) !(Vector Int)

instance Monoid (HeaderErrors c) where
  mempty = HeaderErrors mempty mempty mempty
  mappend (HeaderErrors a1 b1 c1) (HeaderErrors a2 b2 c2) = HeaderErrors
    (mappend a1 a2) (mappend b1 b2) (mappend c1 c2)

-- byteStringChar8 :: Siphon ByteString
-- byteStringChar8 = Siphon
--   escape
--   encodeRow
--   (A.parse (row comma))
--   B.null

encodeRow :: Vector (Escaped ByteString) -> ByteString
encodeRow = id
  . flip B.append (B.singleton newline)
  . B.intercalate (B.singleton comma)
  . V.toList
  . coerce

escape :: ByteString -> Escaped ByteString
escape t = case B.find (\c -> c == newline || c == cr || c == comma || c == doubleQuote) t of
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

-- | Parse a record, not including the terminating line separator. The
-- terminating line separate is not included as the last record in a
-- CSV file is allowed to not have a terminating line separator. You
-- most likely want to use the 'endOfLine' parser in combination with
-- this parser.
-- row :: Word8  -- ^ Field delimiter
--     -> AL.Parser (Vector ByteString)
-- row !delim = rowNoNewline delim <* endOfLine
-- {-# INLINE row #-}
-- 
-- rowNoNewline :: Word8  -- ^ Field delimiter
--              -> AL.Parser (Vector ByteString)
-- rowNoNewline !delim = V.fromList <$!> field delim `sepByDelim1'` delim
-- {-# INLINE rowNoNewline #-}
-- 
-- removeBlankLines :: [Vector ByteString] -> [Vector ByteString]
-- removeBlankLines = filter (not . blankLine)

-- | Parse a field. The field may be in either the escaped or
--   non-escaped format. The return value is unescaped. This
--   parser will consume the comma that comes after a field
--   but not a newline that follows a field. If we are positioned
--   at a newline when it starts, that newline will be consumed
--   and we return CellResultNewline.
field :: Word8 -> AL.Parser (CellResult ByteString)
field !delim = do
  mb <- A.peekWord8
  -- We purposely don't use <|> as we want to commit to the first
  -- choice if we see a double quote.
  case mb of
    Just b
      | b == delim -> do
          bs <- escapedField delim
          return (CellResultData bs)
      | b == 10 || b == 13 -> do
          _ <- eatNewlines
          isEnd <- A.atEnd
          if isEnd
            then return (CellResultNewline EndedYes)
            else return (CellResultNewline EndedNo)
      | otherwise -> do
          bs <- unescapedField delim
          return (CellResultData bs)
    Nothing -> return (CellResultNewline EndedYes)
{-# INLINE field #-}

eatNewlines :: AL.Parser S.ByteString
eatNewlines = A.takeWhile (\x -> x == 10 || x == 13)

escapedField :: Word8 -> AL.Parser S.ByteString
escapedField !delim = do
  _ <- dquote
  -- The scan state is 'True' if the previous character was a double
  -- quote.  We need to drop a trailing double quote left by scan.
  s <- S.init <$> (A.scan False $ \s c -> if c == doubleQuote
                                          then Just (not s)
                                          else if s then Nothing
                                               else Just False)
  A.skip (== delim)
  if doubleQuote `S.elem` s
      then case Z.parse unescape s of
          Right r  -> return r
          Left err -> fail err
      else return s

-- | Consume an unescaped field. If it ends with a newline,
--   leave that in tact. If it ends with a comma, consume the comma.
unescapedField :: Word8 -> AL.Parser S.ByteString
unescapedField !delim = 
  ( A.takeWhile $ \c -> 
    c /= doubleQuote &&
    c /= newline &&
    c /= delim &&
    c /= cr
  ) <* A.skip (== delim)

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

-- | This adds one to the index because text editors consider
--   line number to be one-based, not zero-based.
humanizeSiphonError :: Eq c => (c -> String) -> SiphonError c -> String
humanizeSiphonError toStr (SiphonError ix e) = unlines
  $ ("Decolonnade error on line " ++ show (ix + 1) ++ " of file.")
  : ("Error Category: " ++ descr)
  : map ("  " ++) errDescrs
  where (descr,errDescrs) = prettyRowError toStr e

prettyRowError :: Eq c => (c -> String) -> RowError c -> (String, [String])
prettyRowError toStr x = case x of
  RowErrorParse -> (,) "CSV Parsing"
    [ "The cells were malformed."
    ]
  RowErrorSize reqLen actualLen -> (,) "Row Length"
    [ "Expected the row to have exactly " ++ show reqLen ++ " cells."
    , "The row only has " ++ show actualLen ++ " cells."
    ]
  RowErrorHeaderSize reqLen actualLen -> (,) "Minimum Header Length"
    [ "Expected the row to have at least " ++ show reqLen ++ " cells."
    , "The row only has " ++ show actualLen ++ " cells."
    ]
  RowErrorMalformed column -> (,) "Text Decolonnade"
    [ "Tried to decode input input in column " ++ columnNumToLetters column ++ " text"
    , "There is a mistake in the encoding of the text."
    ]
  RowErrorHeaders dupErrs namedErrs unnamedErrs -> (,) "Missing Headers" $ concat
    [ if V.length namedErrs > 0 then prettyNamedMissingHeaders toStr namedErrs else []
    , if V.length unnamedErrs > 0 then ["Missing unnamed headers"] else []
    , if V.length dupErrs > 0 then prettyHeadingErrors toStr dupErrs else []
    ]
  RowErrorDecode errs -> (,) "Cell Decolonnade" (prettyCellErrors toStr errs)

prettyCellErrors :: (c -> String) -> Vector (CellError c) -> [String]
prettyCellErrors toStr errs = drop 1 $
  flip concatMap errs $ \(CellError ix content) ->
    let str = toStr content in
    [ "-----------"
    , "Column " ++ columnNumToLetters ix
    , "Cell Content Length: " ++ show (Prelude.length str)
    , "Cell Content: " ++ if null str
        then "[empty cell]"
        else str
    ]

prettyNamedMissingHeaders :: (c -> String) -> Vector c -> [String]
prettyNamedMissingHeaders conv missing = concat
  [ concatMap (\h -> ["The header " ++ conv h ++ " was missing."]) missing
  ]

prettyHeadingErrors :: forall c. Eq c
  => (c -> String) -> Vector (Vector (CellError c)) -> [String]
prettyHeadingErrors conv missing = join (V.toList (fmap f missing))
  where
  f :: Vector (CellError c) -> [String]
  f v
    | not (V.null w) && V.all (== V.head w) (V.tail w) =
        [ "The header ["
        , conv (V.head w)
        , "] appears in columns "
        , L.intercalate ", " (V.toList (V.map (\(CellError ix _) -> columnNumToLetters ix) v))
        ]
    | otherwise = multiMsg : V.toList
        (V.map (\(CellError ix content) -> "  Column " ++ columnNumToLetters ix ++ ": " ++ conv content) v)
    where
    w :: Vector c
    w = V.map cellErrorContent v
    multiMsg :: String
    multiMsg = "Multiple headers matched the same predicate:"

columnNumToLetters :: Int -> String
columnNumToLetters i
  | i >= 0 && i < 25 = [chr (i + 65)]
  | otherwise = "Beyond Z. Fix this."

newtype EitherWrap a b = EitherWrap
  { getEitherWrap :: Either a b
  } deriving (Functor)

instance Monoid a => Applicative (EitherWrap a) where
  pure = EitherWrap . Right
  EitherWrap (Left a1) <*> EitherWrap (Left a2) = EitherWrap (Left (mappend a1 a2))
  EitherWrap (Left a1) <*> EitherWrap (Right _) = EitherWrap (Left a1)
  EitherWrap (Right _) <*> EitherWrap (Left a2) = EitherWrap (Left a2)
  EitherWrap (Right f) <*> EitherWrap (Right b) = EitherWrap (Right (f b))

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft _ (Right a) = Right a
mapLeft f (Left a) = Left (f a)

consumeHeaderRowChar8 :: Monad m
  => Stream (Of ByteString) m ()
  -> m (Either (SiphonError ByteString) (Of (Vector ByteString) (Stream (Of ByteString) m ())))
consumeHeaderRowChar8 = consumeHeaderRow (A.parse (field comma)) B.null B.empty (\() -> True)

consumeBodyChar8 :: forall m a. Monad m
  => Int -- ^ index of first row, usually zero or one
  -> Int -- ^ Required row length
  -> Siphon IndexedHeader ByteString a
  -> Stream (Of ByteString) m ()
  -> Stream (Of a) m (Maybe (SiphonError ByteString))
consumeBodyChar8 = consumeBody (A.parse (field comma)) B.null B.empty (\() -> True)

consumeHeaderRow :: forall m r c. Monad m
  => (c -> ATYP.IResult c (CellResult c))
  -> (c -> Bool) -- ^ true if null string
  -> c
  -> (r -> Bool) -- ^ true if termination is acceptable
  -> Stream (Of c) m r
  -> m (Either (SiphonError c) (Of (Vector c) (Stream (Of c) m r)))
consumeHeaderRow parseCell isNull emptyStr isGood s0 = go 0 StrictListNil s0
  where
  go :: Int
     -> StrictList c
     -> Stream (Of c) m r
     -> m (Either (SiphonError c) (Of (Vector c) (Stream (Of c) m r)))
  go !cellsLen !cells !s1 = do
    e <- skipWhile isNull s1
    case e of
      Left r -> return $ if isGood r
        then Right (reverseVectorStrictList cellsLen cells :> return r)
        else Left (SiphonError 0 RowErrorParse)
      Right (c :> s2) -> handleResult cellsLen cells (parseCell c) s2
  handleResult :: Int -> StrictList c 
               -> ATYP.IResult c (CellResult c)
               -> Stream (Of c) m r
               -> m (Either (SiphonError c) (Of (Vector c) (Stream (Of c) m r)))
  handleResult !cellsLen !cells !result s1 = case result of
    ATYP.Fail _ _ _ -> return $ Left $ SiphonError 0 RowErrorParse
    ATYP.Done !c1 !res -> case res of
      -- it might be wrong to ignore whether or not the stream has ended
      CellResultNewline _ -> do
        let v = reverseVectorStrictList cellsLen cells
        return (Right (v :> (SMP.yield c1 >> s1)))
      CellResultData !cd -> if isNull c1
        then go (cellsLen + 1) (StrictListCons cd cells) s1
        else handleResult (cellsLen + 1) (StrictListCons cd cells) (parseCell c1) s1
    ATYP.Partial k -> do
      e <- skipWhile isNull s1
      case e of
        Left r -> handleResult cellsLen cells (k emptyStr) (return r)
        Right (c1 :> s2) -> handleResult cellsLen cells (k c1) s2

consumeBody :: forall m r c a. Monad m
  => (c -> ATYP.IResult c (CellResult c))
  -> (c -> Bool)
  -> c
  -> (r -> Bool) -- ^ True if termination is acceptable. False if it is because of a decoding error.
  -> Int -- ^ index of first row, usually zero or one
  -> Int -- ^ Required row length
  -> Siphon IndexedHeader c a
  -> Stream (Of c) m r
  -> Stream (Of a) m (Maybe (SiphonError c))
consumeBody parseCell isNull emptyStr isGood row0 reqLen siphon s0 = go row0 0 StrictListNil s0
  where
  go :: Int -> Int -> StrictList c -> Stream (Of c) m r -> Stream (Of a) m (Maybe (SiphonError c))
  go !row !cellsLen !cells !s1 = do
    e <- lift (skipWhile isNull s1)
    case e of
      Left r -> return $ if isGood r
        then Nothing
        else Just (SiphonError row RowErrorParse)
      Right (c :> s2) -> handleResult row cellsLen cells (parseCell c) s2
  handleResult :: Int -> Int -> StrictList c 
               -> ATYP.IResult c (CellResult c)
               -> Stream (Of c) m r
               -> Stream (Of a) m (Maybe (SiphonError c))
  handleResult !row !cellsLen !cells !result s1 = case result of
    ATYP.Fail _ _ _ -> return $ Just $ SiphonError row RowErrorParse
    ATYP.Done !c1 !res -> case res of
      CellResultNewline ended -> do
        case decodeRow row (reverseVectorStrictList cellsLen cells) of
          Left err -> return (Just err)
          Right a -> do
            SMP.yield a
            case ended of
              EndedYes -> do
                e <- lift (SM.inspect s1)
                case e of
                  Left r -> return $ if isGood r
                    then Nothing
                    else Just (SiphonError row RowErrorParse)
                  Right _ -> error "siphon: logical error, stream should be exhausted"
              EndedNo -> if isNull c1 
                then go (row + 1) 0 StrictListNil s1
                else handleResult (row + 1) 0 StrictListNil (parseCell c1) s1
      CellResultData !cd -> if isNull c1
        then go row (cellsLen + 1) (StrictListCons cd cells) s1
        else handleResult row (cellsLen + 1) (StrictListCons cd cells) (parseCell c1) s1
    ATYP.Partial k -> do
      e <- lift (skipWhile isNull s1)
      case e of
        Left r -> handleResult row cellsLen cells (k emptyStr) (return r)
        Right (c1 :> s2) -> handleResult row cellsLen cells (k c1) s2
  decodeRow :: Int -> Vector c -> Either (SiphonError c) a
  decodeRow rowIx v =
    let vlen = V.length v in
    if vlen /= reqLen
      then Left $ SiphonError rowIx $ RowErrorSize reqLen vlen
      else uncheckedRunWithRow rowIx siphon v

-- | You must pass the length of the list and as the first argument.
reverseVectorStrictList :: Int -> StrictList c -> Vector c
reverseVectorStrictList _ _ = error "write me"

skipWhile :: forall m a r. Monad m
  => (a -> Bool)
  -> Stream (Of a) m r
  -> m (Either r (Of a (Stream (Of a) m r)))
skipWhile f = go where
  go :: Stream (Of a) m r
     -> m (Either r (Of a (Stream (Of a) m r)))
  go s1 = do
    e <- SM.inspect s1
    case e of
      Left _ -> return e
      Right (a :> s2) -> if f a
        then go s2
        else return e

-- | Strict in the spine and in the values
data StrictList a = StrictListNil | StrictListCons !a !(StrictList a)

-- | This function uses 'unsafeIndex' to access
--   elements of the 'Vector'.
uncheckedRunWithRow ::
     Int
  -> Siphon IndexedHeader c a
  -> Vector c
  -> Either (SiphonError c) a
uncheckedRunWithRow i d v = mapLeft (SiphonError i . RowErrorDecode) (uncheckedRun d v)

-- | This function does not check to make sure that the indicies in
--   the 'Decolonnade' are in the 'Vector'. Only use this if you have
--   already verified that none of the indices in the siphon are
--   out of the bounds.
uncheckedRun :: forall c a.
     Siphon IndexedHeader c a
  -> Vector c
  -> Either (Vector (CellError c)) a
uncheckedRun dc v = getEitherWrap (go dc)
  where
  go :: forall b.
        Siphon IndexedHeader c b
     -> EitherWrap (Vector (CellError c)) b
  go (SiphonPure b) = EitherWrap (Right b)
  go (SiphonAp (IndexedHeader ix _) decode apNext) =
    let rnext = go apNext
        content = V.unsafeIndex v ix
        rcurrent = maybe
          (Left (V.singleton (CellError ix content)))
          Right
          (decode content)
    in rnext <*> (EitherWrap rcurrent)

siphonLength :: forall f c a. Siphon f c a -> Int
siphonLength = go 0 where
  go :: forall b. Int -> Siphon f c b -> Int
  go !a (SiphonPure _) = a
  go !a (SiphonAp _ _ apNext) = go (a + 1) apNext

maxIndex :: forall c a. Siphon IndexedHeader c a -> Int
maxIndex = go 0 where
  go :: forall b. Int -> Siphon IndexedHeader c b -> Int
  go !ix (SiphonPure _) = ix
  go !ix1 (SiphonAp (IndexedHeader ix2 _) _ apNext) =
    go (max ix1 ix2) apNext

