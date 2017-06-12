{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module Siphon.Decoding 
  ( mkParseError
  , headlessPipe
  , indexedPipe
  , headedPipe
  , consumeGeneral
  , pipeGeneral
  , convertDecodeError
  ) where

import Siphon.Types
import Colonnade (Headed(..),Headless(..))
import Siphon.Internal (row,comma)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Pipes (yield,Pipe,Consumer',Producer,await)
import Data.Vector (Vector)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Char (chr)
import qualified Data.Vector as Vector
import qualified Data.Attoparsec.ByteString as AttoByteString
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Attoparsec.Types as Atto

mkParseError :: Int -> [String] -> String -> DecolonnadeRowError f content
mkParseError i ctxs msg = id
  $ DecolonnadeRowError i
  $ RowErrorParse $ concat
    [ "Contexts: ["
    , concat ctxs
    , "], Error Message: ["
    , msg
    , "]"
    ]

-- | This is a convenience function for working with @pipes-text@.
--   It will convert a UTF-8 decoding error into a `DecolonnadeRowError`,
--   so the pipes can be properly chained together.
convertDecodeError :: String -> Either (Producer ByteString m ()) () -> Maybe (DecolonnadeRowError f c)
convertDecodeError encodingName (Left _) = Just (DecolonnadeRowError 0 (RowErrorMalformed encodingName))
convertDecodeError _ (Right ()) = Nothing

-- | This is seldom useful but is included for completeness.
headlessPipe :: Monad m
  => Siphon c
  -> Decolonnade Headless c a
  -> Pipe c a m (DecolonnadeRowError Headless c)
headlessPipe sd decoding = uncheckedPipe requiredLength 0 sd indexedDecoding Nothing
  where
  indexedDecoding = headlessToIndexed decoding
  requiredLength = decLength indexedDecoding

indexedPipe :: Monad m
  => Siphon c
  -> Decolonnade (Indexed Headless) c a
  -> Pipe c a m (DecolonnadeRowError Headless c)
indexedPipe sd decoding = do
  e <- consumeGeneral 0 sd mkParseError
  case e of
    Left err -> return err
    Right (firstRow, mleftovers) ->
      let req = maxIndex decoding
          vlen = Vector.length firstRow
       in if vlen < req
            then return (DecolonnadeRowError 0 (RowErrorMinSize req vlen))
            else case uncheckedRun decoding firstRow of
              Left cellErr -> return $ DecolonnadeRowError 0 $ RowErrorDecode cellErr
              Right a -> do
                yield a
                uncheckedPipe vlen 1 sd decoding mleftovers


headedPipe :: (Monad m, Eq c)
  => Siphon c
  -> Decolonnade Headed c a
  -> Pipe c a m (DecolonnadeRowError Headed c)
headedPipe sd decoding = do
  e <- consumeGeneral 0 sd mkParseError
  case e of
    Left err -> return err
    Right (headers, mleftovers) ->
      case headedToIndexed headers decoding of
        Left headingErrs -> return (DecolonnadeRowError 0 (RowErrorHeading headingErrs))
        Right indexedDecoding ->
          let requiredLength = Vector.length headers
           in uncheckedPipe requiredLength 1 sd indexedDecoding mleftovers


uncheckedPipe :: Monad m
  => Int -- ^ expected length of each row
  -> Int -- ^ index of first row, usually zero or one
  -> Siphon c
  -> Decolonnade (Indexed f) c a
  -> Maybe c
  -> Pipe c a m (DecolonnadeRowError f c)
uncheckedPipe requiredLength ix sd d mleftovers =
  pipeGeneral ix sd mkParseError checkedRunWithRow mleftovers
  where
  checkedRunWithRow rowIx v =
    let vlen = Vector.length v in
    if vlen /= requiredLength
      then Left $ DecolonnadeRowError rowIx
                $ RowErrorSize requiredLength vlen
      else uncheckedRunWithRow rowIx d v

consumeGeneral :: Monad m
  => Int
  -> Siphon c
  -> (Int -> [String] -> String -> e)
  -> Consumer' c m (Either e (Vector c, Maybe c))
consumeGeneral ix (Siphon _ _ parse isNull) wrapParseError = do
  c <- awaitSkip isNull
  handleResult (parse c)
  where
  go k = do
    c <- awaitSkip isNull
    handleResult (k c)
  handleResult r = case r of
    Atto.Fail _ ctxs msg -> return $ Left
      $ wrapParseError ix ctxs msg
    Atto.Done c v ->
      let mcontent = if isNull c
            then Nothing
            else Just c
       in return (Right (v,mcontent))
    Atto.Partial k -> go k

pipeGeneral :: Monad m
  => Int -- ^ index of first row, usually zero or one
  -> Siphon c
  -> (Int -> [String] -> String -> e)
  -> (Int -> Vector c -> Either e a)
  -> Maybe c -- ^ leftovers that should be handled first
  -> Pipe c a m e
pipeGeneral initIx (Siphon _ _ parse isNull) wrapParseError decodeRow mleftovers =
  case mleftovers of
    Nothing -> go1 initIx
    Just leftovers -> handleResult initIx (parse leftovers)
  where
  go1 !ix = do
    c1 <- awaitSkip isNull
    handleResult ix (parse c1)
  go2 !ix c1 = handleResult ix (parse c1)
  go3 !ix k = do
    c1 <- awaitSkip isNull
    handleResult ix (k c1)
  handleResult !ix r = case r of
    Atto.Fail _ ctxs msg -> return $ wrapParseError ix ctxs msg
    Atto.Done c1 v -> do
      case decodeRow ix v of
        Left err -> return err
        Right r -> do
          yield r
          let ixNext = ix + 1
          if isNull c1 then go1 ixNext else go2 ixNext c1
    Atto.Partial k -> go3 ix k

awaitSkip :: Monad m
          => (a -> Bool)
          -> Consumer' a m a
awaitSkip f = go where
  go = do
    a <- await
    if f a then go else return a

-- | Converts the content type of a 'Decolonnade'. The @'Contravariant' f@
-- constraint means that @f@ can be 'Headless' but not 'Headed'.
contramapContent :: forall c1 c2 f a. Contravariant f => (c2 -> c1) -> Decolonnade f c1 a -> Decolonnade f c2 a
contramapContent f = go
  where
  go :: forall b. Decolonnade f c1 b -> Decolonnade f c2 b
  go (DecolonnadePure x) = DecolonnadePure x
  go (DecolonnadeAp h decode apNext) =
    DecolonnadeAp (contramap f h) (decode . f) (go apNext)

headless :: (content -> Either String a) -> Decolonnade Headless content a
headless f = DecolonnadeAp Headless f (DecolonnadePure id)

headed :: content -> (content -> Either String a) -> Decolonnade Headed content a
headed h f = DecolonnadeAp (Headed h) f (DecolonnadePure id)

indexed :: Int -> (content -> Either String a) -> Decolonnade (Indexed Headless) content a
indexed ix f = DecolonnadeAp (Indexed ix Headless) f (DecolonnadePure id)

maxIndex :: forall f c a. Decolonnade (Indexed f) c a -> Int
maxIndex = go 0 where
  go :: forall b. Int -> Decolonnade (Indexed f) c b -> Int
  go !ix (DecolonnadePure _) = ix
  go !ix1 (DecolonnadeAp (Indexed ix2 _) decode apNext) =
    go (max ix1 ix2) apNext

-- | This function uses 'unsafeIndex' to access
--   elements of the 'Vector'.
uncheckedRunWithRow ::
     Int
  -> Decolonnade (Indexed f) content a
  -> Vector content
  -> Either (DecolonnadeRowError f content) a
uncheckedRunWithRow i d v = mapLeft (DecolonnadeRowError i . RowErrorDecode) (uncheckedRun d v)

-- | This function does not check to make sure that the indicies in
--   the 'Decolonnade' are in the 'Vector'.
uncheckedRun :: forall content a f.
                Decolonnade (Indexed f) content a
             -> Vector content
             -> Either (DecolonnadeCellErrors f content) a
uncheckedRun dc v = getEitherWrap (go dc)
  where
  go :: forall b.
        Decolonnade (Indexed f) content b
     -> EitherWrap (DecolonnadeCellErrors f content) b
  go (DecolonnadePure b) = EitherWrap (Right b)
  go (DecolonnadeAp ixed@(Indexed ix h) decode apNext) =
    let rnext = go apNext
        content = Vector.unsafeIndex v ix
        rcurrent = mapLeft (DecolonnadeCellErrors . Vector.singleton . DecolonnadeCellError content ixed) (decode content)
    in rnext <*> (EitherWrap rcurrent)

headlessToIndexed :: forall c a.
  Decolonnade Headless c a -> Decolonnade (Indexed Headless) c a
headlessToIndexed = go 0 where
  go :: forall b. Int -> Decolonnade Headless c b -> Decolonnade (Indexed Headless) c b
  go !ix (DecolonnadePure a) = DecolonnadePure a
  go !ix (DecolonnadeAp Headless decode apNext) =
    DecolonnadeAp (Indexed ix Headless) decode (go (ix + 1) apNext)

decLength :: forall f c a. Decolonnade f c a -> Int
decLength = go 0 where
  go :: forall b. Int -> Decolonnade f c b -> Int
  go !a (DecolonnadePure _) = a
  go !a (DecolonnadeAp _ _ apNext) = go (a + 1) apNext

-- | Maps over a 'Decolonnade' that expects headers, converting these
--   expected headers into the indices of the columns that they
--   correspond to.
headedToIndexed :: forall content a. Eq content
                => Vector content -- ^ Headers in the source document
                -> Decolonnade Headed content a -- ^ Decolonnade that contains expected headers
                -> Either (HeadingErrors content) (Decolonnade (Indexed Headed) content a)
headedToIndexed v = getEitherWrap . go
  where
  go :: forall b. Eq content
     => Decolonnade Headed content b
     -> EitherWrap (HeadingErrors content) (Decolonnade (Indexed Headed) content b)
  go (DecolonnadePure b) = EitherWrap (Right (DecolonnadePure b))
  go (DecolonnadeAp hd@(Headed h) decode apNext) =
    let rnext = go apNext
        ixs = Vector.elemIndices h v
        ixsLen = Vector.length ixs
        rcurrent
          | ixsLen == 1 = Right (Vector.unsafeIndex ixs 0)
          | ixsLen == 0 = Left (HeadingErrors (Vector.singleton h) Vector.empty)
          | otherwise   = Left (HeadingErrors Vector.empty (Vector.singleton (h,ixsLen)))
    in (\ix ap -> DecolonnadeAp (Indexed ix hd) decode ap)
       <$> EitherWrap rcurrent
       <*> rnext

-- | This adds one to the index because text editors consider
--   line number to be one-based, not zero-based.
prettyError :: (c -> String) -> DecolonnadeRowError f c -> String
prettyError toStr (DecolonnadeRowError ix e) = unlines
  $ ("Decolonnade error on line " ++ show (ix + 1) ++ " of file.")
  : ("Error Category: " ++ descr)
  : map ("  " ++) errDescrs
  where (descr,errDescrs) = prettyRowError toStr e

prettyRowError :: (content -> String) -> RowError f content -> (String, [String])
prettyRowError toStr x = case x of
  RowErrorParse err -> (,) "CSV Parsing"
    [ "The line could not be parsed into cells correctly."
    , "Original parser error: " ++ err
    ]
  RowErrorSize reqLen actualLen -> (,) "Row Length"
    [ "Expected the row to have exactly " ++ show reqLen ++ " cells."
    , "The row only has " ++ show actualLen ++ " cells."
    ]
  RowErrorMinSize reqLen actualLen -> (,) "Row Min Length"
    [ "Expected the row to have at least " ++ show reqLen ++ " cells."
    , "The row only has " ++ show actualLen ++ " cells."
    ]
  RowErrorMalformed enc -> (,) "Text Decolonnade"
    [ "Tried to decode the input as " ++ enc ++ " text"
    , "There is a mistake in the encoding of the text."
    ]
  RowErrorHeading errs -> (,) "Header" (prettyHeadingErrors toStr errs)
  RowErrorDecode errs -> (,) "Cell Decolonnade" (prettyCellErrors toStr errs)

prettyCellErrors :: (c -> String) -> DecolonnadeCellErrors f c -> [String]
prettyCellErrors toStr (DecolonnadeCellErrors errs) = drop 1 $
  flip concatMap errs $ \(DecolonnadeCellError content (Indexed ix _) msg) ->
    let str = toStr content in
    [ "-----------"
    , "Column " ++ columnNumToLetters ix
    , "Original parse error: " ++ msg
    , "Cell Content Length: " ++ show (Prelude.length str)
    , "Cell Content: " ++ if null str
        then "[empty cell]"
        else str
    ]

prettyHeadingErrors :: (c -> String) -> HeadingErrors c -> [String]
prettyHeadingErrors conv (HeadingErrors missing duplicates) = concat
  [ concatMap (\h -> ["The header " ++ conv h ++ " was missing."]) missing
  , concatMap (\(h,n) -> ["The header " ++ conv h ++ " occurred " ++ show n ++ " times."]) duplicates
  ]

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





