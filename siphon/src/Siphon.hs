{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

-- | Build CSVs using the abstractions provided in the @colonnade@ library, and 
--   parse CSVs using 'Siphon', which is the dual of 'Colonnade'.
--   Read the documentation for @colonnade@ before reading the documentation
--   for @siphon@. All of the examples on this page assume a common set of
--   imports that are provided at the bottom of this page.
module Siphon
  ( -- * Encode CSV
    encodeCsv
  , encodeCsvStream
  , encodeCsvUtf8
  , encodeCsvStreamUtf8
    -- * Decode CSV
  , decodeCsvUtf8
    -- * Build Siphon
  , headed
  , headless
  , indexed
    -- * Types
  , Siphon
  , SiphonError
  , Indexed(..)
    -- * Utility
  , humanizeSiphonError
    -- * Imports
    -- $setup
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
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text as T
import qualified Data.List as L
import qualified Streaming as SM
import qualified Streaming.Prelude as SMP
import qualified Data.Attoparsec.Types as ATYP
import qualified Colonnade.Encode as CE
import qualified Data.Vector.Mutable as MV
import qualified Data.ByteString.Builder as BB

import Control.Monad.Trans.Class
import Data.Functor.Identity (Identity(..))
import Data.ByteString.Builder (toLazyByteString,byteString)
import Data.Attoparsec.ByteString.Char8 (char, endOfInput, string)
import Data.Word (Word8)
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Char (chr)
import Data.Text.Encoding (decodeUtf8')
import Streaming (Stream,Of(..))
import Data.Vector.Mutable (MVector)
import Control.Monad.ST
import Data.Text (Text)

newtype Escaped c = Escaped { getEscaped :: c }
data Ended = EndedYes | EndedNo
  deriving (Show)
data CellResult c = CellResultData !c | CellResultNewline !c !Ended
  deriving (Show)

decodeCsvUtf8 :: Monad m 
  => Siphon CE.Headed ByteString a
  -> Stream (Of ByteString) m () -- ^ encoded csv
  -> Stream (Of a) m (Maybe SiphonError)
decodeCsvUtf8 headedSiphon s1 = do
  e <- lift (consumeHeaderRowUtf8 s1)
  case e of
    Left err -> return (Just err)
    Right (v :> s2) -> case headedToIndexed utf8ToStr v headedSiphon of
      Left err -> return (Just err)
      Right ixedSiphon -> do
        let requiredLength = V.length v
        consumeBodyUtf8 1 requiredLength ixedSiphon s2

encodeCsvStreamUtf8 :: (Monad m, CE.Headedness h)
  => CE.Colonnade h a ByteString
  -> Stream (Of a) m r
  -> Stream (Of ByteString) m r
encodeCsvStreamUtf8 =
  encodeCsvInternal escapeChar8 (B.singleton comma) (B.singleton newline)

-- | Streaming variant of 'encodeCsv'. This is particularly useful
--   when you need to produce millions of rows without having them
--   all loaded into memory at the same time.
encodeCsvStream :: (Monad m, CE.Headedness h)
  => CE.Colonnade h a Text
  -> Stream (Of a) m r
  -> Stream (Of Text) m r
encodeCsvStream =
  encodeCsvInternal textEscapeChar8 (T.singleton ',') (T.singleton '\n')

-- | Encode a collection to a CSV as a text 'TB.Builder'. For example,
--   we can take the following columnar encoding of a person:
--
-- >>> :{
-- let colPerson :: Colonnade Headed Person Text
--     colPerson = mconcat
--       [ C.headed "Name" name
--       , C.headed "Age" (T.pack . show . age)
--       , C.headed "Company" (fromMaybe "N/A" . company)
--       ]
-- :}
--
-- And we have the following people whom we wish to encode
-- in this way:
--
-- >>> :{
-- let people :: [Person]
--     people =
--       [ Person "Chao" 26 (Just "Tectonic, Inc.")
--       , Person "Elsie" 41 (Just "Globex Corporation")
--       , Person "Arabella" 19 Nothing
--       ]
-- :}
--
-- We pair the encoding with the rows to get a CSV:
--
-- >>> LTIO.putStr (TB.toLazyText (encodeCsv colPerson people))
-- Name,Age,Company
-- Chao,26,"Tectonic, Inc."
-- Elsie,41,Globex Corporation
-- Arabella,19,N/A
encodeCsv :: (Foldable f, CE.Headedness h)
  => CE.Colonnade h a Text -- ^ Tablular encoding
  -> f a -- ^ Value of each row
  -> TB.Builder
encodeCsv enc = 
  textStreamToBuilder . encodeCsvStream enc . SMP.each

-- | Encode a collection to a CSV as a bytestring 'BB.Builder'.
encodeCsvUtf8 :: (Foldable f, CE.Headedness h)
  => CE.Colonnade h a ByteString -- ^ Tablular encoding
  -> f a -- ^ Value of each row
  -> BB.Builder
encodeCsvUtf8 enc =
  streamToBuilder . encodeCsvStreamUtf8 enc . SMP.each

streamToBuilder :: Stream (Of ByteString) Identity () -> BB.Builder
streamToBuilder s = SM.destroy s
  (\(bs :> bb) -> BB.byteString bs <> bb) runIdentity (\() -> mempty)

textStreamToBuilder :: Stream (Of Text) Identity () -> TB.Builder
textStreamToBuilder s = SM.destroy s
  (\(bs :> bb) -> TB.fromText bs <> bb) runIdentity (\() -> mempty)

encodeCsvInternal :: (Monad m, CE.Headedness h)
  => (c -> Escaped c)
  -> c -- ^ separator
  -> c -- ^ newline
  -> CE.Colonnade h a c
  -> Stream (Of a) m r
  -> Stream (Of c) m r
encodeCsvInternal escapeFunc separatorStr newlineStr colonnade s = do
  case CE.headednessExtract of
    Just toContent -> encodeHeader toContent escapeFunc separatorStr newlineStr colonnade
    Nothing -> return ()
  encodeRows escapeFunc separatorStr newlineStr colonnade s

encodeHeader :: Monad m
  => (h c -> c)
  -> (c -> Escaped c)
  -> c -- ^ separator
  -> c -- ^ newline
  -> CE.Colonnade h a c
  -> Stream (Of c) m ()
encodeHeader toContent escapeFunc separatorStr newlineStr colonnade = do
  let (vs,ws) = V.splitAt 1 (CE.getColonnade colonnade)
  -- we only need to do this split because the first cell
  -- gets treated differently than the others. It does not
  -- get a separator added before it.
  V.forM_ vs $ \(CE.OneColonnade h _) -> do
    SMP.yield (getEscaped (escapeFunc (toContent h)))
  V.forM_ ws $ \(CE.OneColonnade h _) -> do
    SMP.yield separatorStr
    SMP.yield (getEscaped (escapeFunc (toContent h)))
  SMP.yield newlineStr

mapStreamM :: Monad m
  => (a -> Stream (Of b) m x)
  -> Stream (Of a) m r
  -> Stream (Of b) m r
mapStreamM f = SM.concats . SM.mapsM (\(a :> s) -> return (f a >> return s))

encodeRows :: Monad m
  => (c -> Escaped c)
  -> c -- ^ separator
  -> c -- ^ newline
  -> CE.Colonnade f a c
  -> Stream (Of a) m r
  -> Stream (Of c) m r
encodeRows escapeFunc separatorStr newlineStr colonnade = mapStreamM $ \a -> do
  let (vs,ws) = V.splitAt 1 (CE.getColonnade colonnade)
  -- we only need to do this split because the first cell
  -- gets treated differently than the others. It does not
  -- get a separator added before it.
  V.forM_ vs $ \(CE.OneColonnade _ encode) -> SMP.yield (getEscaped (escapeFunc (encode a)))
  V.forM_ ws $ \(CE.OneColonnade _ encode) -> do
    SMP.yield separatorStr
    SMP.yield (getEscaped (escapeFunc (encode a)))
  SMP.yield newlineStr

data IndexedHeader a = IndexedHeader
  { indexedHeaderIndexed :: {-# UNPACK #-} !Int
  , indexedHeaderHeader :: !a
  } 

-- | Maps over a 'Decolonnade' that expects headers, converting these
--   expected headers into the indices of the columns that they
--   correspond to.
headedToIndexed :: forall c a. Eq c
  => (c -> T.Text)
  -> Vector c -- ^ Headers in the source document
  -> Siphon CE.Headed c a -- ^ Decolonnade that contains expected headers
  -> Either SiphonError (Siphon IndexedHeader c a)
headedToIndexed toStr v =
    mapLeft (\(HeaderErrors a b c) -> SiphonError 0 (RowErrorHeaders a b c)) 
  . getEitherWrap
  . go
  where
  go :: forall b.
        Siphon CE.Headed c b
     -> EitherWrap HeaderErrors (Siphon IndexedHeader c b)
  go (SiphonPure b) = EitherWrap (Right (SiphonPure b))
  go (SiphonAp (CE.Headed h) decode apNext) =
    let rnext = go apNext
        ixs = V.elemIndices h v
        ixsLen = V.length ixs
        rcurrent
          | ixsLen == 1 = Right (ixs V.! 0) -- (V.unsafeIndex ixs 0)
          | ixsLen == 0 = Left (HeaderErrors V.empty (V.singleton (toStr h)) V.empty)
          | otherwise =
              let dups = V.singleton (V.map (\ix -> CellError ix (toStr (v V.! ix) {- (V.unsafeIndex v ix) -} )) ixs)
               in Left (HeaderErrors dups V.empty V.empty)
    in (\ix nextSiphon -> SiphonAp (IndexedHeader ix h) decode nextSiphon)
       <$> EitherWrap rcurrent
       <*> rnext

data HeaderErrors = HeaderErrors !(Vector (Vector CellError)) !(Vector T.Text) !(Vector Int)

instance Monoid HeaderErrors where
  mempty = HeaderErrors mempty mempty mempty
  mappend (HeaderErrors a1 b1 c1) (HeaderErrors a2 b2 c2) = HeaderErrors
    (mappend a1 a2) (mappend b1 b2) (mappend c1 c2)

-- byteStringChar8 :: Siphon ByteString
-- byteStringChar8 = Siphon
--   escape
--   encodeRow
--   (A.parse (row comma))
--   B.null

escapeChar8 :: ByteString -> Escaped ByteString
escapeChar8 t = case B.find (\c -> c == newline || c == cr || c == comma || c == doubleQuote) t of
  Nothing -> Escaped t
  Just _  -> escapeAlways t

textEscapeChar8 :: Text -> Escaped Text
textEscapeChar8 t = case T.find (\c -> c == '\n' || c == '\r' || c == ',' || c == '"') t of
  Nothing -> Escaped t
  Just _  -> textEscapeAlways t

-- This implementation is definitely suboptimal.
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

-- Suboptimal for similar reason as escapeAlways.
textEscapeAlways :: Text -> Escaped Text
textEscapeAlways t = Escaped $ LT.toStrict $ TB.toLazyText $
     TB.singleton '"'
  <> T.foldl
      (\ acc b -> acc <> if b == '"'
          then TB.fromString "\"\""
          else TB.singleton b
      )
      mempty
      t
  <> TB.singleton '"'

-- Parse a record, not including the terminating line separator. The
-- terminating line separate is not included as the last record in a
-- CSV file is allowed to not have a terminating line separator. You
-- most likely want to use the 'endOfLine' parser in combination with
-- this parser.
--
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
      | b == doubleQuote -> do
          (bs,tc) <- escapedField
          case tc of
            TrailCharComma -> return (CellResultData bs)
            TrailCharNewline -> return (CellResultNewline bs EndedNo)
            TrailCharEnd -> return (CellResultNewline bs EndedYes)
      | b == 10 || b == 13 -> do
          _ <- eatNewlines
          isEnd <- A.atEnd
          if isEnd
            then return (CellResultNewline B.empty EndedYes)
            else return (CellResultNewline B.empty EndedNo)
      | otherwise -> do
          (bs,tc) <- unescapedField delim
          case tc of
            TrailCharComma -> return (CellResultData bs)
            TrailCharNewline -> return (CellResultNewline bs EndedNo)
            TrailCharEnd -> return (CellResultNewline bs EndedYes)
    Nothing -> return (CellResultNewline B.empty EndedYes)
{-# INLINE field #-}

eatNewlines :: AL.Parser S.ByteString
eatNewlines = A.takeWhile (\x -> x == 10 || x == 13)

escapedField :: AL.Parser (S.ByteString,TrailChar)
escapedField = do
  _ <- dquote
  -- The scan state is 'True' if the previous character was a double
  -- quote.  We need to drop a trailing double quote left by scan.
  s <- S.init <$>
    ( A.scan False $ \s c ->
      if c == doubleQuote
        then Just (not s)
        else if s
          then Nothing
          else Just False
    )
  mb <- A.peekWord8
  trailChar <- case mb of
    Just b
      | b == comma -> A.anyWord8 >> return TrailCharComma
      | b == newline || b == cr -> A.anyWord8 >> return TrailCharNewline
      | otherwise -> fail "encountered double quote after escaped field"
    Nothing -> return TrailCharEnd
  if doubleQuote `S.elem` s
    then case Z.parse unescape s of
      Right r  -> return (r,trailChar)
      Left err -> fail err
    else return (s,trailChar)

data TrailChar = TrailCharNewline | TrailCharComma | TrailCharEnd

-- | Consume an unescaped field. If it ends with a newline,
--   leave that in tact. If it ends with a comma, consume the comma.
unescapedField :: Word8 -> AL.Parser (S.ByteString,TrailChar)
unescapedField !delim = do
  bs <- A.takeWhile $ \c -> 
    c /= doubleQuote &&
    c /= newline &&
    c /= delim &&
    c /= cr
  mb <- A.peekWord8
  case mb of
    Just b
      | b == comma -> A.anyWord8 >> return (bs,TrailCharComma)
      | b == newline || b == cr -> A.anyWord8 >> return (bs,TrailCharNewline)
      | otherwise -> fail "encountered double quote in unescaped field"
    Nothing -> return (bs,TrailCharEnd)

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

doubleQuote, newline, cr, comma :: Word8
doubleQuote = 34
newline = 10
cr = 13
comma = 44

-- | This adds one to the index because text editors consider
--   line number to be one-based, not zero-based.
humanizeSiphonError :: SiphonError -> String
humanizeSiphonError (SiphonError ix e) = unlines
  $ ("Decolonnade error on line " ++ show (ix + 1) ++ " of file.")
  : ("Error Category: " ++ descr)
  : map ("  " ++) errDescrs
  where (descr,errDescrs) = prettyRowError e

prettyRowError :: RowError -> (String, [String])
prettyRowError x = case x of
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
    [ if V.length namedErrs > 0 then prettyNamedMissingHeaders namedErrs else []
    , if V.length unnamedErrs > 0 then ["Missing unnamed headers"] else []
    , if V.length dupErrs > 0 then prettyHeadingErrors dupErrs else []
    ]
  RowErrorDecode errs -> (,) "Cell Decolonnade" (prettyCellErrors errs)

prettyCellErrors :: Vector CellError -> [String]
prettyCellErrors errs = drop 1 $
  flip concatMap errs $ \(CellError ix content) ->
    let str = T.unpack content in
    [ "-----------"
    , "Column " ++ columnNumToLetters ix
    , "Cell Content Length: " ++ show (Prelude.length str)
    , "Cell Content: " ++ if null str
        then "[empty cell]"
        else str
    ]

prettyNamedMissingHeaders :: Vector T.Text -> [String]
prettyNamedMissingHeaders missing = concat
  [ concatMap (\h -> ["The header " ++ T.unpack h ++ " was missing."]) missing
  ]

prettyHeadingErrors :: Vector (Vector CellError) -> [String]
prettyHeadingErrors missing = join (V.toList (fmap f missing))
  where
  f :: Vector CellError -> [String]
  f v
    | not (V.null w) && V.all (== V.head w) (V.tail w) =
        [ "The header ["
        , T.unpack (V.head w)
        , "] appears in columns "
        , L.intercalate ", " (V.toList (V.map (\(CellError ix _) -> columnNumToLetters ix) v))
        ]
    | otherwise = multiMsg : V.toList
        (V.map (\(CellError ix content) -> "  Column " ++ columnNumToLetters ix ++ ": " ++ T.unpack content) v)
    where
    w :: Vector T.Text
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

consumeHeaderRowUtf8 :: Monad m
  => Stream (Of ByteString) m ()
  -> m (Either SiphonError (Of (Vector ByteString) (Stream (Of ByteString) m ())))
consumeHeaderRowUtf8 = consumeHeaderRow utf8ToStr (A.parse (field comma)) B.null B.empty (\() -> True)

consumeBodyUtf8 :: forall m a. Monad m
  => Int -- ^ index of first row, usually zero or one
  -> Int -- ^ Required row length
  -> Siphon IndexedHeader ByteString a
  -> Stream (Of ByteString) m ()
  -> Stream (Of a) m (Maybe SiphonError)
consumeBodyUtf8 = consumeBody utf8ToStr
  (A.parse (field comma)) B.null B.empty (\() -> True)

utf8ToStr :: ByteString -> T.Text
utf8ToStr = either (\_ -> T.empty) id . decodeUtf8'

consumeHeaderRow :: forall m r c. Monad m
  => (c -> T.Text)
  -> (c -> ATYP.IResult c (CellResult c))
  -> (c -> Bool) -- ^ true if null string
  -> c
  -> (r -> Bool) -- ^ true if termination is acceptable
  -> Stream (Of c) m r
  -> m (Either SiphonError (Of (Vector c) (Stream (Of c) m r)))
consumeHeaderRow toStr parseCell isNull emptyStr isGood s0 = go 0 StrictListNil s0
  where
  go :: Int
     -> StrictList c
     -> Stream (Of c) m r
     -> m (Either SiphonError (Of (Vector c) (Stream (Of c) m r)))
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
               -> m (Either SiphonError (Of (Vector c) (Stream (Of c) m r)))
  handleResult !cellsLen !cells !result s1 = case result of
    ATYP.Fail _ _ _ -> return $ Left $ SiphonError 0 RowErrorParse
    ATYP.Done !c1 !res -> case res of
      -- it might be wrong to ignore whether or not the stream has ended
      CellResultNewline cd _ -> do
        let v = reverseVectorStrictList (cellsLen + 1) (StrictListCons cd cells)
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
  => (c -> T.Text)
  -> (c -> ATYP.IResult c (CellResult c))
  -> (c -> Bool)
  -> c
  -> (r -> Bool) -- ^ True if termination is acceptable. False if it is because of a decoding error.
  -> Int -- ^ index of first row, usually zero or one
  -> Int -- ^ Required row length
  -> Siphon IndexedHeader c a
  -> Stream (Of c) m r
  -> Stream (Of a) m (Maybe SiphonError)
consumeBody toStr parseCell isNull emptyStr isGood row0 reqLen siphon s0 =
  go row0 0 StrictListNil s0
  where
  go :: Int -> Int -> StrictList c -> Stream (Of c) m r -> Stream (Of a) m (Maybe SiphonError)
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
               -> Stream (Of a) m (Maybe SiphonError)
  handleResult !row !cellsLen !cells !result s1 = case result of
    ATYP.Fail _ _ _ -> return $ Just $ SiphonError row RowErrorParse
    ATYP.Done !c1 !res -> case res of
      CellResultNewline !cd !ended -> do
        case decodeRow row (reverseVectorStrictList (cellsLen + 1) (StrictListCons cd cells)) of
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
  decodeRow :: Int -> Vector c -> Either SiphonError a
  decodeRow rowIx v =
    let vlen = V.length v in
    if vlen /= reqLen
      then Left $ SiphonError rowIx $ RowErrorSize reqLen vlen
      else uncheckedRunWithRow toStr rowIx siphon v

-- | You must pass the length of the list and as the first argument.
--   Passing the wrong length will lead to an error.
reverseVectorStrictList :: forall c. Int -> StrictList c -> Vector c
reverseVectorStrictList len sl0 = V.create $ do
  mv <- MV.new len
  go1 mv
  return mv
  where
  go1 :: forall s. MVector s c -> ST s ()
  go1 !mv = go2 0 sl0
    where
    go2 :: Int -> StrictList c -> ST s ()
    go2 _ StrictListNil = return ()
    go2 !ix (StrictListCons c slNext) = do
      MV.write mv ix c
      go2 (ix + 1) slNext


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
     (c -> T.Text)
  -> Int
  -> Siphon IndexedHeader c a
  -> Vector c
  -> Either SiphonError a
uncheckedRunWithRow toStr i d v =
  mapLeft (SiphonError i . RowErrorDecode) (uncheckedRun toStr d v)

-- | This function does not check to make sure that the indicies in
--   the 'Decolonnade' are in the 'Vector'. Only use this if you have
--   already verified that none of the indices in the siphon are
--   out of the bounds.
uncheckedRun :: forall c a.
     (c -> T.Text)
  -> Siphon IndexedHeader c a
  -> Vector c
  -> Either (Vector CellError) a
uncheckedRun toStr dc v = getEitherWrap (go dc)
  where
  go :: forall b.
        Siphon IndexedHeader c b
     -> EitherWrap (Vector CellError) b
  go (SiphonPure b) = EitherWrap (Right b)
  go (SiphonAp (IndexedHeader ix _) decode apNext) =
    let rnext = go apNext
        content = v V.! ix -- V.unsafeIndex v ix
        rcurrent = maybe
          (Left (V.singleton (CellError ix (toStr content))))
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

-- | Uses the argument to parse a CSV column.
headless :: (c -> Maybe a) -> Siphon CE.Headless c a
headless f = SiphonAp CE.Headless f (SiphonPure id)

-- | Uses the second argument to parse a CSV column whose 
--   header content matches the first column exactly.
headed :: c -> (c -> Maybe a) -> Siphon CE.Headed c a
headed h f = SiphonAp (CE.Headed h) f (SiphonPure id)

-- | Uses the second argument to parse a CSV column that
--   is positioned at the index given by the first argument.
indexed :: Int -> (c -> Maybe a) -> Siphon Indexed c a
indexed ix f = SiphonAp (Indexed ix) f (SiphonPure id)

-- $setup
--
-- This code is copied from the head section. It has to be
-- run before every set of tests.
--
-- >>> :set -XOverloadedStrings
-- >>> import Siphon (Siphon)
-- >>> import Colonnade (Colonnade,Headed)
-- >>> import qualified Siphon as S
-- >>> import qualified Colonnade as C
-- >>> import qualified Data.Text as T
-- >>> import Data.Text (Text)
-- >>> import qualified Data.Text.Lazy.IO as LTIO
-- >>> import qualified Data.Text.Lazy.Builder as LB
-- >>> import Data.Maybe (fromMaybe)
-- >>> data Person = Person { name :: Text, age :: Int, company :: Maybe Text}

