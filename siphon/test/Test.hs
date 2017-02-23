{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main (main) where

import Test.QuickCheck                      (Gen, Arbitrary(..), choose, elements)
import Test.HUnit                           (Assertion,(@?=))
import Test.Framework                       (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit       (testCase)
import Data.ByteString                      (ByteString)
import Data.Text                            (Text)
import GHC.Generics                         (Generic)
import Data.Either.Combinators
import Siphon.Types
import Data.Functor.Identity
import Data.Functor.Contravariant           (contramap)
import Data.Functor.Contravariant.Divisible (divided,conquered)
import Colonnade (headed,headless,Colonnade,Headed,Headless)
import Data.Profunctor (lmap)
import qualified Data.Text                  as Text
import qualified Data.ByteString.Builder    as Builder
import qualified Data.ByteString.Lazy       as LByteString
import qualified Data.ByteString            as ByteString
import qualified Data.ByteString.Char8      as BC8
import qualified Colonnade                  as Colonnade
import qualified Siphon.Encoding            as SE
import qualified Siphon.Decoding            as SD
import qualified Siphon.Content             as SC
import qualified Pipes.Prelude              as Pipes
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Lazy.Builder.Int as TBuilder
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
  , testGroup "Text encode/decode"
    [ testCase "Headless Encoding (int,char,bool)"
        $ runTestScenario
            SC.text
            SE.pipe
            encodingW
            "4,c,false\n"
    , testCase "Headless Encoding (Foo,Foo,Foo)"
        $ runCustomTestScenario
            SC.text
            SE.pipe
            encodingY
            (FooA,FooA,FooC)
            "Simple,Simple,\"More\"\"Escaped,\"\"\"\"Chars\"\n"
    , testProperty "Headless Isomorphism (Foo,Foo,Foo)"
        $ propIsoPipe $
          (SE.pipe SC.text encodingY)
          >->
          (void $ SD.headlessPipe SC.text decodingY)
    ]
  ]

data Foo = FooA | FooB | FooC
  deriving (Generic,Eq,Ord,Show,Read,Bounded,Enum)

instance Arbitrary Foo where
  arbitrary = elements [minBound..maxBound]

fooToString :: Foo -> String
fooToString x = case x of
  FooA -> "Simple"
  FooB -> "With,Escaped\nChars"
  FooC -> "More\"Escaped,\"\"Chars"

encodeFoo :: (String -> c) -> Foo -> c
encodeFoo f = f . fooToString

fooFromString :: String -> Either String Foo
fooFromString x = case x of
  "Simple" -> Right FooA
  "With,Escaped\nChars" -> Right FooB
  "More\"Escaped,\"\"Chars" -> Right FooC
  _ -> Left "failed to decode Foo"

decodeFoo :: (c -> String) -> c -> Either String Foo
decodeFoo f = fooFromString . f

decodingA :: Decolonnade Headless ByteString (Int,Char,Bool)
decodingA = (,,)
  <$> SD.headless dbInt
  <*> SD.headless dbChar
  <*> SD.headless dbBool

decodingB :: Decolonnade Headed ByteString (Int,Char,Bool)
decodingB = (,,)
  <$> SD.headed "number" dbInt
  <*> SD.headed "letter" dbChar
  <*> SD.headed "boolean" dbBool

encodingA :: Colonnade Headless (Int,Char,Bool) ByteString
encodingA = mconcat
  [ lmap fst3 (headless ebInt)
  , lmap snd3 (headless ebChar)
  , lmap thd3 (headless ebBool)
  ]

encodingW :: Colonnade Headless (Int,Char,Bool) Text
encodingW = mconcat
  [ lmap fst3 (headless etInt)
  , lmap snd3 (headless etChar)
  , lmap thd3 (headless etBool)
  ]

encodingY :: Colonnade Headless (Foo,Foo,Foo) Text
encodingY = mconcat
  [ lmap fst3 (headless $ encodeFoo Text.pack)
  , lmap snd3 (headless $ encodeFoo Text.pack)
  , lmap thd3 (headless $ encodeFoo Text.pack)
  ]

decodingY :: Decolonnade Headless Text (Foo,Foo,Foo)
decodingY = (,,)
  <$> SD.headless (decodeFoo Text.unpack)
  <*> SD.headless (decodeFoo Text.unpack)
  <*> SD.headless (decodeFoo Text.unpack)

encodingB :: Colonnade Headed (Int,Char,Bool) ByteString
encodingB = mconcat
  [ lmap fst3 (headed "number" ebInt)
  , lmap snd3 (headed "letter" ebChar)
  , lmap thd3 (headed "boolean" ebBool)
  ]

encodingC :: Colonnade Headed (Int,Char,Bool) ByteString
encodingC = mconcat
  [ lmap thd3 $ headed "boolean" ebBool
  , lmap snd3 $ headed "letter" ebChar
  ]

tripleToPairs :: (a,b,c) -> (a,(b,(c,())))
tripleToPairs (a,b,c) = (a,(b,(c,())))

propIsoPipe :: Eq a => Pipe a a Identity () -> [a] -> Bool
propIsoPipe p as = (Pipes.toList $ each as >-> p) == as

runTestScenario :: (Monoid c, Eq c, Show c)
  => Siphon c
  -> (Siphon c -> Colonnade f (Int,Char,Bool) c -> Pipe (Int,Char,Bool) c Identity ())
  -> Colonnade f (Int,Char,Bool) c
  -> c
  -> Assertion
runTestScenario s p e c =
  ( mconcat $ Pipes.toList $
    Pipes.yield (4,'c',False) >-> p s e
  ) @?= c

runCustomTestScenario :: (Monoid c, Eq c, Show c)
  => Siphon c
  -> (Siphon c -> Colonnade f a c -> Pipe a c Identity ())
  -> Colonnade f a c
  -> a
  -> c
  -> Assertion
runCustomTestScenario s p e a c =
  ( mconcat $ Pipes.toList $
    Pipes.yield a >-> p s e
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


dbChar :: ByteString -> Either String Char
dbChar b = case BC8.length b of
  1 -> Right (BC8.head b)
  0 -> Left "cannot decode Char from empty bytestring"
  _ -> Left "cannot decode Char from multi-character bytestring"

dbInt :: ByteString -> Either String Int
dbInt b = do
  (a,bsRem) <- maybe (Left "could not parse int") Right (BC8.readInt b)
  if ByteString.null bsRem
    then Right a
    else Left "found extra characters after int"

dbBool :: ByteString -> Either String Bool
dbBool b
  | b == BC8.pack "true" = Right True
  | b == BC8.pack "false" = Right False
  | otherwise = Left "must be true or false"

ebChar :: Char -> ByteString
ebChar = BC8.singleton

ebInt :: Int -> ByteString
ebInt = LByteString.toStrict
    . Builder.toLazyByteString 
    . Builder.intDec

ebBool :: Bool -> ByteString
ebBool x = case x of
  True -> BC8.pack "true"
  False -> BC8.pack "false"

ebByteString :: ByteString -> ByteString
ebByteString = id


etChar :: Char -> Text
etChar = Text.singleton

etInt :: Int -> Text
etInt = LText.toStrict
    . TBuilder.toLazyText
    . TBuilder.decimal

etText :: Text -> Text
etText = id

etBool :: Bool -> Text
etBool x = case x of
  True -> Text.pack "true"
  False -> Text.pack "false"

