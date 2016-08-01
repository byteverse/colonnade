{-# LANGUAGE OverloadedStrings #-}

module Geolite.Csv where

import Data.Text (Text)
import Pipes (Pipe)
import Colonnade.Types
import Geolite.Types

import qualified Data.Text as Text
import qualified Net.IPv4.Range.Text as IPv4RangeText
import qualified Data.Text.Read as TextRead
import qualified Siphon.Decoding as SD
import qualified Siphon.Content as SC
import qualified Colonnade.Decoding.Text as CDT
import qualified Colonnade.Decoding as CD

cities :: Monad m => Pipe Text City m (DecodingRowError Headed Text)
cities = SD.headedPipe SC.text decodingCity

blocks :: Monad m => Pipe Text Block m (DecodingRowError Headed Text)
blocks = SD.headedPipe SC.text decodingBlock

decodingCity :: Decoding Headed Text City
decodingCity = City
  <$> fmap GeonameId (CD.headed "geoname_id" CDT.int)
  <*> CD.headed "locale_code" CDT.text
  <*> CD.headed "continent_code" CDT.text
  <*> CD.headed "continent_name" CDT.text
  <*> CD.headed "country_iso_code" CDT.text
  <*> CD.headed "country_name" CDT.text
  <*> CD.headed "subdivision_1_iso_code" CDT.text
  <*> CD.headed "subdivision_1_name" CDT.text
  <*> CD.headed "subdivision_2_iso_code" CDT.text
  <*> CD.headed "subdivision_2_name" CDT.text
  <*> CD.headed "city_name" CDT.text
  <*> CD.headed "metro_code" (CDT.optional CDT.int)
  <*> CD.headed "time_zone" CDT.text

decodingBlock :: Decoding Headed Text Block
decodingBlock = Block
  <$> CD.headed "network" IPv4RangeText.decodeEither
  <*> CD.headed "geoname_id"
        (CDT.optional $ CDT.map GeonameId CDT.int)
  <*> CD.headed "registered_country_geoname_id"
        (CDT.optional $ CDT.map GeonameId CDT.int)
  <*> CD.headed "represented_country_geoname_id"
        (CDT.optional $ CDT.map GeonameId CDT.int)
  <*> CD.headed "is_anonymous_proxy" (CDT.trueFalse "1" "0")
  <*> CD.headed "is_satellite_provider" (CDT.trueFalse "1" "0")
  <*> CD.headed "postal_code" CDT.text
  <*> CD.headed "latitude"
        (CDT.optional $ CDT.fromReader TextRead.rational)
  <*> CD.headed "longitude"
        (CDT.optional $ CDT.fromReader TextRead.rational)
  <*> CD.headed "accuracy_radius"
        (CDT.optional CDT.int)


