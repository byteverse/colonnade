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
import Siphon.Types
import Data.Functor.Identity
import Data.Functor.Contravariant           (contramap)
import Data.Functor.Contravariant.Divisible (divided,conquered)
import qualified Data.ByteString.Builder    as Builder
import qualified Data.ByteString.Lazy       as LByteString
import qualified Data.ByteString            as ByteString
import qualified Data.ByteString.Char8      as BC8
import qualified Colonnade.Decoding         as Decoding
import qualified Colonnade.Encoding         as Encoding
import qualified Colonnade.Decoding.ByteString.Char8 as CDB
import qualified Colonnade.Encoding.ByteString.Char8 as CEB
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
    [ testCase "Headless Encoding (int,char,bool)"
        $ runTestScenario 
            SC.byteStringChar8 
            SE.pipe
            encodingA 
            "4,c,false\n"
    , testProperty "Headless Isomorphism (int,char,bool)"
        $ propIsoPipe $
          (SE.pipe SC.byteStringChar8 encodingA)
          >->
          (void $ SD.headlessPipe SC.byteStringChar8 decodingA)
    , testCase "Headed Encoding (int,char,bool)"
        $ runTestScenario 
            SC.byteStringChar8 
            SE.headedPipe
            encodingB
            $ ByteString.concat
              [ "number,letter,boolean\n"
              , "4,c,false\n"
              ]
    , testCase "Headed Encoding (int,char,bool) monoidal building"
        $ runTestScenario 
            SC.byteStringChar8 
            SE.headedPipe
            encodingC
            $ ByteString.concat
              [ "boolean,letter\n"
              , "false,c\n"
              ]
    , testProperty "Headed Isomorphism (int,char,bool)"
        $ propIsoPipe $
          (SE.headedPipe SC.byteStringChar8 encodingB)
          >->
          (void $ SD.headedPipe SC.byteStringChar8 decodingB)
    ]
  ]

decodingA :: Decoding Headless ByteString (Int,Char,Bool)
decodingA = (,,)
  <$> Decoding.headless CDB.int
  <*> Decoding.headless CDB.char
  <*> Decoding.headless CDB.bool

decodingB :: Decoding Headed ByteString (Int,Char,Bool)
decodingB = (,,)
  <$> Decoding.headed "number" CDB.int
  <*> Decoding.headed "letter" CDB.char
  <*> Decoding.headed "boolean" CDB.bool

encodingA :: Encoding Headless ByteString (Int,Char,Bool)
encodingA = contramap tripleToPairs
  $ divided (Encoding.headless CEB.int)
  $ divided (Encoding.headless CEB.char)
  $ divided (Encoding.headless CEB.bool)
  $ conquered 

encodingB :: Encoding Headed ByteString (Int,Char,Bool)
encodingB = contramap tripleToPairs
  $ divided (Encoding.headed "number" CEB.int)
  $ divided (Encoding.headed "letter" CEB.char)
  $ divided (Encoding.headed "boolean" CEB.bool)
  $ conquered

encodingC :: Encoding Headed ByteString (Int,Char,Bool)
encodingC = mconcat
  [ contramap thd3 $ Encoding.headed "boolean" CEB.bool
  , contramap snd3 $ Encoding.headed "letter" CEB.char
  ]

tripleToPairs :: (a,b,c) -> (a,(b,(c,())))
tripleToPairs (a,b,c) = (a,(b,(c,())))

propIsoPipe :: Eq a => Pipe a a Identity () -> [a] -> Bool
propIsoPipe p as = (Pipes.toList $ each as >-> p) == as

runTestScenario :: (Monoid c, Eq c, Show c)
  => Siphon c
  -> (Siphon c -> Encoding f c (Int,Char,Bool) -> Pipe (Int,Char,Bool) c Identity ())
  -> Encoding f c (Int,Char,Bool) 
  -> c 
  -> Assertion
runTestScenario s p e c = 
  ( mconcat $ Pipes.toList $
    Pipes.yield (4,'c',False) >-> p s e
  ) @?= c

-- testEncodingA :: Assertion
-- testEncodingA = runTestScenario encodingA "4,c,false\n"

propEncodeDecodeIso :: Eq a => (a -> b) -> (b -> Maybe a) -> a -> Bool
propEncodeDecodeIso f g a = g (f a) == Just a

propMatching :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
propMatching f g a = f a == g a


-- | Take the first item out of a 3 element tuple
fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

-- | Take the second item out of a 3 element tuple
snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

-- | Take the third item out of a 3 element tuple
thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c

