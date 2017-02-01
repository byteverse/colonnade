{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns        #-}
module Colonnade.Decoding where

import Colonnade.Internal (EitherWrap(..),mapLeft)
import Colonnade.Types
import Data.Functor.Contravariant
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Char (chr)

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

length :: forall f c a. Decolonnade f c a -> Int
length = go 0 where
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



