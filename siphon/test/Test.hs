{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main (main) where

import Test.QuickCheck (Gen, Arbitrary(..), choose, elements, Property)
import Test.QuickCheck.Property (Result, succeeded, exception)
import Test.HUnit (Assertion,(@?=))
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Either.Combinators
import Siphon.Types
import Data.Functor.Identity
import Data.Functor.Contravariant           (contramap)
import Data.Functor.Contravariant.Divisible (divided,conquered)
import Colonnade (headed,headless,Colonnade,Headed,Headless)
import Data.Profunctor (lmap)
import Streaming (Stream,Of(..))
import Control.Exception
import Debug.Trace
import Data.Word (Word8)
import Data.Char (ord)
import qualified Data.Text as Text
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString as B
import qualified Colonnade as Colonnade
import qualified Siphon as S
import qualified Streaming.Prelude as SMP
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Text.Lazy.Builder.Int as TBuilder

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "ByteString encode/decode"
    [ testCase "Headed Encoding (int,char,bool)"
        $ runTestScenario [(4,intToWord8 (ord 'c'),False)]
            S.encodeCsvStreamUtf8
            encodingB
            $ ByteString.concat
              [ "number,letter,boolean\n"
              , "4,c,false\n"
              ]
    , testCase "Headed Encoding (int,char,bool) monoidal building"
        $ runTestScenario [(4,'c',False)]
            S.encodeCsvStreamUtf8
            encodingC
            $ ByteString.concat
              [ "boolean,letter\n"
              , "false,c\n"
              ]
    , testCase "Headed Encoding (escaped characters)"
        $ runTestScenario ["bob","there,be,commas","the \" quote"]
            S.encodeCsvStreamUtf8
            encodingF
            $ ByteString.concat
              [ "name\n"
              , "bob\n"
              , "\"there,be,commas\"\n"
              , "\"the \"\" quote\"\n"
              ]
    , testCase "Headed Decoding (int,char,bool)"
        $ ( runIdentity . SMP.toList )
            ( S.decodeCsvUtf8 decodingB
              ( mapM_ (SMP.yield . BC8.singleton) $ concat
                [ "number,letter,boolean\n"
                , "244,z,true\n"
                ]
              )
            ) @?= ([(244,intToWord8 (ord 'z'),True)] :> Nothing)
    , testCase "Headed Decoding (escaped characters, one big chunk)"
        $ ( runIdentity . SMP.toList )
            ( S.decodeCsvUtf8 decodingF
              ( SMP.yield $ BC8.pack $ concat
                [ "name\n"
                , "drew\n"
                , "\"martin, drew\"\n"
                ]
              )
            ) @?= (["drew","martin, drew"] :> Nothing)
    , testCase "Headed Decoding (escaped characters, character per chunk)"
        $ ( runIdentity . SMP.toList )
            ( S.decodeCsvUtf8 decodingF
              ( mapM_ (SMP.yield . BC8.singleton) $ concat
                [ "name\n"
                , "drew\n"
                , "\"martin, drew\"\n"
                ]
              )
            ) @?= (["drew","martin, drew"] :> Nothing)
    , testProperty "Headed Isomorphism (int,char,bool)"
        $ propIsoStream BC8.unpack
          (S.decodeCsvUtf8 decodingB)
          (S.encodeCsvStreamUtf8 encodingB)
    ]
  ]

intToWord8 :: Int -> Word8
intToWord8 = fromIntegral

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

fooFromString :: String -> Maybe Foo
fooFromString x = case x of
  "Simple" -> Just FooA
  "With,Escaped\nChars" -> Just FooB
  "More\"Escaped,\"\"Chars" -> Just FooC
  _ -> Nothing

decodeFoo :: (c -> String) -> c -> Maybe Foo
decodeFoo f = fooFromString . f

decodingA :: Siphon Headless ByteString (Int,Char,Bool)
decodingA = (,,)
  <$> S.headless dbInt
  <*> S.headless dbChar
  <*> S.headless dbBool

decodingB :: Siphon Headed ByteString (Int,Word8,Bool)
decodingB = (,,)
  <$> S.headed "number" dbInt
  <*> S.headed "letter" dbWord8
  <*> S.headed "boolean" dbBool

decodingF :: Siphon Headed ByteString ByteString
decodingF = S.headed "name" Just


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

decodingY :: Siphon Headless Text (Foo,Foo,Foo)
decodingY = (,,)
  <$> S.headless (decodeFoo Text.unpack)
  <*> S.headless (decodeFoo Text.unpack)
  <*> S.headless (decodeFoo Text.unpack)

encodingF :: Colonnade Headed ByteString ByteString
encodingF = headed "name" id

encodingB :: Colonnade Headed (Int,Word8,Bool) ByteString
encodingB = mconcat
  [ lmap fst3 (headed "number" ebInt)
  , lmap snd3 (headed "letter" ebWord8)
  , lmap thd3 (headed "boolean" ebBool)
  ]

encodingC :: Colonnade Headed (Int,Char,Bool) ByteString
encodingC = mconcat
  [ lmap thd3 $ headed "boolean" ebBool
  , lmap snd3 $ headed "letter" ebChar
  ]

tripleToPairs :: (a,b,c) -> (a,(b,(c,())))
tripleToPairs (a,b,c) = (a,(b,(c,())))

propIsoStream :: (Eq a, Show a, Monoid c)
  => (c -> String)
  -> (Stream (Of c) Identity () -> Stream (Of a) Identity (Maybe SiphonError))
  -> (Stream (Of a) Identity () -> Stream (Of c) Identity ())
  -> [a]
  -> Result
propIsoStream toStr decode encode as =
  let asNew :> m = runIdentity $ SMP.toList $ decode $ encode $ SMP.each as
   in case m of
        Nothing -> if as == asNew
          then succeeded
          else exception ("expected " ++ show as ++ " but got " ++ show asNew) myException
        Just err ->
          let csv = toStr $ mconcat $ runIdentity $ SMP.toList_ $ encode $ SMP.each as
           in exception (S.humanizeSiphonError err ++ "\nGenerated CSV\n" ++ csv) myException

data MyException = MyException
  deriving (Show,Read,Eq)
instance Exception MyException

myException :: SomeException
myException = SomeException MyException

runTestScenario :: (Monoid c, Eq c, Show c, Eq a, Show a)
  => [a]
  -> (Colonnade f a c -> Stream (Of a) Identity () -> Stream (Of c) Identity ())
  -> Colonnade f a c
  -> c
  -> Assertion
runTestScenario as p e c =
  ( mconcat (runIdentity (SMP.toList_ (p e (mapM_ SMP.yield as))))
  ) @?= c

-- runCustomTestScenario :: (Monoid c, Eq c, Show c)
--   => Siphon c
--   -> (Siphon c -> Colonnade f a c -> Pipe a c Identity ())
--   -> Colonnade f a c
--   -> a
--   -> c
--   -> Assertion
-- runCustomTestScenario s p e a c =
--   ( mconcat $ Pipes.toList $
--     Pipes.yield a >-> p s e
--   ) @?= c

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


dbChar :: ByteString -> Maybe Char
dbChar b = case BC8.length b of
  1 -> Just (BC8.head b)
  _ -> Nothing

dbWord8 :: ByteString -> Maybe Word8
dbWord8 b = case B.length b of
  1 -> Just (B.head b)
  _ -> Nothing

dbInt :: ByteString -> Maybe Int
dbInt b = do
  (a,bsRem) <- BC8.readInt b
  if ByteString.null bsRem
    then Just a
    else Nothing

dbBool :: ByteString -> Maybe Bool
dbBool b
  | b == BC8.pack "true" = Just True
  | b == BC8.pack "false" = Just False
  | otherwise = Nothing

ebChar :: Char -> ByteString
ebChar = BC8.singleton

ebWord8 :: Word8 -> ByteString
ebWord8 = B.singleton

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

