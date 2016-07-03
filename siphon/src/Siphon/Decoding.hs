{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE BangPatterns #-}

module Siphon.Decoding where

import Siphon.Types
import Colonnade.Types
import Siphon.Internal (row,comma)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Pipes (yield,Pipe,Consumer',Producer,await)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Colonnade.Decoding as Decoding
import qualified Data.Attoparsec.ByteString as AttoByteString
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Attoparsec.Types as Atto

byteStringChar8 :: SiphonDecoding ByteString ByteString
byteStringChar8 = SiphonDecoding
  (AttoByteString.parse (row comma))
  ByteString.null

-- unrow :: c1 -> (Vector c2,c1)
-- 
-- row :: _
--     -> Decoding (Indexed f) c a
--     -> Vector c
--     -> Either DecodingErrors a

-- decodeVectorPipe ::
--      Monad m
--   => Decoding (Indexed f) c a
--   -> Pipe (Vector c) a m ()
-- decodeVectorPipe 

mkParseError :: Int -> [String] -> String -> DecodingRowError f content
mkParseError i ctxs msg = id
  $ DecodingRowError i
  $ RowErrorParse $ concat
    [ "Contexts: ["
    , concat ctxs
    , "], Error Message: ["
    , msg
    , "]"
    ]

headlessPipe :: Monad m
  => SiphonDecoding c1 c2 
  -> Decoding Headless c2 a
  -> Pipe c1 a m (DecodingRowError Headless c2)
headlessPipe sd decoding = uncheckedPipe requiredLength 0 sd indexedDecoding Nothing
  where
  indexedDecoding = Decoding.headlessToIndexed decoding
  requiredLength = Decoding.length indexedDecoding

headedPipe :: (Monad m, Eq c2)
  => SiphonDecoding c1 c2 
  -> Decoding Headed c2 a
  -> Pipe c1 a m (DecodingRowError Headed c2)
headedPipe sd decoding = do
  (headers, mleftovers) <- consumeGeneral sd mkParseError 
  case Decoding.headedToIndexed headers decoding of
    Left headingErrs -> return (DecodingRowError 0 (RowErrorHeading headingErrs))
    Right indexedDecoding -> 
      let requiredLength = Decoding.length indexedDecoding
       in uncheckedPipe requiredLength 1 sd indexedDecoding mleftovers
  

uncheckedPipe :: Monad m
  => Int -- ^ expected length of each row
  -> Int -- ^ index of first row, usually zero or one
  -> SiphonDecoding c1 c2 
  -> Decoding (Indexed f) c2 a
  -> Maybe c1
  -> Pipe c1 a m (DecodingRowError f c2)
uncheckedPipe requiredLength ix sd d mleftovers = 
  pipeGeneral ix sd mkParseError checkedRunWithRow mleftovers
  where
  checkedRunWithRow rowIx v = 
    let vlen = Vector.length v in
    if vlen /= requiredLength
      then Left $ DecodingRowError rowIx 
                $ RowErrorSize requiredLength vlen
      else Decoding.uncheckedRunWithRow rowIx d v

consumeGeneral :: Monad m
  => SiphonDecoding c1 c2
  -> (Int -> [String] -> String -> e)
  -> Consumer' c1 m (Vector c2, Maybe c1)
consumeGeneral = error "ahh"

pipeGeneral :: Monad m
  => Int -- ^ index of first row, usually zero or one
  -> SiphonDecoding c1 c2 
  -> (Int -> [String] -> String -> e)
  -> (Int -> Vector c2 -> Either e a)
  -> Maybe c1 -- ^ leftovers that should be handled first
  -> Pipe c1 a m e
pipeGeneral initIx (SiphonDecoding parse isNull) wrapParseError decodeRow mleftovers = 
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
          if isNull c1 then go1 ix else go2 ix c1
    Atto.Partial k -> go3 ix k

awaitSkip :: Monad m
          => (a -> Bool)
          -> Consumer' a m a
awaitSkip f = go where
  go = do
    a <- await
    if f a then go else return a 


