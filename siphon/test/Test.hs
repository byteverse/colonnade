{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.QuickCheck                      (Gen, Arbitrary(..), choose)
import Test.HUnit                           (Assertion,(@?=))
import Test.Framework                       (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit       (testCase)
import Data.ByteString                      (ByteString)
import Data.Either.Combinators
import Colonnade.Types
import Data.Functor.Identity
import Data.Functor.Contravariant           (contramap)
import Data.Functor.Contravariant.Divisible (divided,conquered)
import qualified Data.ByteString.Builder    as Builder
import qualified Data.ByteString.Lazy       as LByteString
import qualified Data.ByteString            as ByteString
import qualified Data.ByteString.Char8      as BC8
import qualified Colonnade.Decoding         as Decoding
import qualified Colonnade.Encoding         as Encoding
import qualified Siphon.Encoding            as SE
import qualified Siphon.Decoding            as SD
import qualified Siphon.Content             as SC
import qualified Pipes.Prelude              as Pipes
import Pipes

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "ByteString encode/decode"
    [ testCase "Headless Encoding (int,char,bool)" testEncodingA
    , testProperty "Headless Isomorphism (int,char,bool)"
        $ propIsoPipe $
          (SE.pipe SC.byteStringChar8 encodingA)
          >->
          (void $ SD.headlessPipe SC.byteStringChar8 decodingA)
    ]
  ]

byteStringDecodeInt :: ByteString -> Either String Int
byteStringDecodeInt b = do
  (a,bsRem) <- maybe (Left "could not parse int") Right (BC8.readInt b)
  if ByteString.null bsRem
    then Right a
    else Left "found extra characters after int"

byteStringDecodeChar :: ByteString -> Either String Char
byteStringDecodeChar b = case BC8.length b of
  1 -> Right (BC8.head b)
  0 -> Left "cannot decode Char from empty bytestring"
  _ -> Left "cannot decode Char from multi-character bytestring"

byteStringDecodeBool :: ByteString -> Either String Bool
byteStringDecodeBool b
  | b == BC8.pack "true" = Right True
  | b == BC8.pack "false" = Right False
  | otherwise = Left "must be true or false"

byteStringEncodeChar :: Char -> ByteString
byteStringEncodeChar = BC8.singleton

byteStringEncodeInt :: Int -> ByteString
byteStringEncodeInt = LByteString.toStrict
                    . Builder.toLazyByteString
                    . Builder.intDec

byteStringEncodeBool :: Bool -> ByteString
byteStringEncodeBool x = case x of
  True -> BC8.pack "true"
  False -> BC8.pack "false"


decodingA :: Decoding Headless ByteString (Int,Char,Bool)
decodingA = (,,)
  <$> Decoding.headless byteStringDecodeInt
  <*> Decoding.headless byteStringDecodeChar
  <*> Decoding.headless byteStringDecodeBool

encodingA :: Encoding Headless ByteString (Int,Char,Bool)
encodingA = contramap tripleToPairs
  $ divided (Encoding.headless byteStringEncodeInt)
  $ divided (Encoding.headless byteStringEncodeChar)
  $ divided (Encoding.headless byteStringEncodeBool)
  $ conquered

tripleToPairs :: (a,b,c) -> (a,(b,(c,())))
tripleToPairs (a,b,c) = (a,(b,(c,())))

propIsoPipe :: Eq a => Pipe a a Identity () -> [a] -> Bool
propIsoPipe p as = (Pipes.toList $ each as >-> p) == as

testEncodingA :: Assertion
testEncodingA =
  ( ByteString.concat $ Pipes.toList $
    Pipes.yield (4,'c',False) >-> SE.pipe SC.byteStringChar8 encodingA
  ) @?= "4,c,false\n"


propEncodeDecodeIso :: Eq a => (a -> b) -> (b -> Maybe a) -> a -> Bool
propEncodeDecodeIso f g a = g (f a) == Just a

propMatching :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
propMatching f g a = f a == g a

