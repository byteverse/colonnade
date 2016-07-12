{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.HUnit                           (Assertion,(@?=))
import Test.Framework                       (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit       (testCase)
import Geolite.Csv                          (cities,blocks)
import Data.Text                            (Text)
import Colonnade.Types
import Siphon.Types
import Data.Functor.Identity
import System.IO                            (withFile,IOMode(ReadMode))
import qualified Data.Text                  as Text
import qualified Pipes.Prelude              as Pipes
import qualified Pipes.ByteString           as PB
import qualified Pipes.Text.Encoding        as PT
import qualified Siphon.Decoding            as SD
import Pipes

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Geolite CSV Decoding"
    [ testCase "Network Blocks" $ streamFileWith
        "data/GeoLite2-City-Blocks-IPv4.small.csv"
        blocks
    , testCase "English City Locations" $ streamFileWith
        "data/GeoLite2-City-Locations-en.small.csv"
        cities
    ]
  ]

streamFileWith ::
     String
  -> Pipe Text a IO (DecodingRowError Headed Text)
  -> Assertion
streamFileWith filename decodingPipe = do
  r <- withFile filename ReadMode $ \h -> runEffect $
        fmap SD.csvResultFromEither (PT.decode (PT.utf8 . PT.eof) $ PB.fromHandle h)
    >-> fmap SD.csvResultFromDecodingRowError decodingPipe
    >-> Pipes.drain
  r @?= CsvResultSuccess

