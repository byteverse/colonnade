{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.HUnit                           (Assertion,(@?=),assertBool,assertFailure)
import Test.Framework                       (defaultMainWithOpts, interpretArgsOrExit,
                                             testGroup, Test)
import Test.Framework.Providers.HUnit       (testCase)
import Test.Framework.Runners.TestPattern   (parseTestPattern)
import Test.Framework.Runners.Options       (RunnerOptions'(..))
import Geolite.Csv                          (cities,blocks)
import Data.Text                            (Text)
import Colonnade.Types
import Siphon.Types
import Data.Functor.Identity
import Control.Monad                        (unless)
import System.Environment                   (getArgs)
import System.Directory                     (doesDirectoryExist)
import System.IO                            (withFile,IOMode(ReadMode))
import qualified Data.Text                  as Text
import qualified Pipes.Prelude              as Pipes
import qualified Pipes.ByteString           as PB
import qualified Pipes.Text.Encoding        as PT
import qualified Siphon.Decoding            as SD
import qualified Colonnade.Decoding         as Decoding
import Pipes

------------------------------------------------
-- The default behavior of this test suite is to
-- test the CSV decoders against small samples of
-- the GeoLite2 databases. These small samples are
-- included as part of this repository. If you give
-- this test suite an argument named "large", it
-- will run against the full CSVs, which are around
-- 350MB. These are not included
-- as part of the repository, so they need to be
-- downloaded. The script found in
-- scripts/load-full-databases will download the full
-- archive, decompress it, and move the files to
-- the appropriate directory for this test suite
-- to run on them.
-----------------------------------------------

main :: IO ()
main = do
  xs <- getArgs
  ropts' <- interpretArgsOrExit xs
  let ropts = ropts'
        { ropt_test_patterns = case ropt_test_patterns ropts' of
            Nothing -> Just [parseTestPattern "small"]
            Just xs -> Just xs
        }
  defaultMainWithOpts tests ropts

tests :: [Test]
tests = flip concatMap ["small","large"] $ \size ->
  [ testGroup size
    [ testCase "Network Blocks" $ streamFileWith
        ("data/" ++ size ++ "/GeoLite2-City-Blocks-IPv4.csv")
        blocks
    , testCase "English City Locations" $ streamFileWith
        ("data/" ++ size ++ "/GeoLite2-City-Locations-en.csv")
        cities
    , testCase "Japanese City Locations" $ streamFileWith
        ("data/" ++ size ++ "/GeoLite2-City-Locations-ja.csv")
        cities
    ]
  ]

streamFileWith ::
     String
  -> Pipe Text a IO (DecodingRowError Headed Text)
  -> Assertion
streamFileWith filename decodingPipe = do
  r <- withFile filename ReadMode $ \h -> runEffect $
        fmap (SD.convertDecodeError "utf-8") (PT.decode (PT.utf8 . PT.eof) $ PB.fromHandle h)
    >-> fmap Just decodingPipe
    >-> Pipes.drain
  case r of
    Nothing -> assertBool "impossible" True
    Just err -> assertFailure (Decoding.prettyError Text.unpack err)

-- let dirPiece = case xs of
--       ["full"] -> "large/"
--       _        -> "small/"
--     fullDirName = "data/" ++ dirPiece
--     errMsg = concat
--       [ "The "
--       , fullDirName
--       , " directory does not exist in the geolite project"
--       ]
