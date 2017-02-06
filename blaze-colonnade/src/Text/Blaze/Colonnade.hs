-- | Build HTML tables using @blaze-html@ and @colonnade@.
-- 
module Text.Blaze.Colonnade
  ( -- * Apply
    -- $build
    encodeHeadedHtmlTable
  , encodeHeadlessHtmlTable
  , encodeHeadedCellTable
  , encodeHeadlessCellTable
  , encodeTable
    -- * Cell
    -- $build
  , Cell(..)
  , htmlCell
  , stringCell
  , textCell
  , lazyTextCell
  , builderCell
    -- * Discussion
    -- $discussion
  ) where

import Text.Blaze (Attribute,(!))
import Text.Blaze.Html (Html, toHtml)
import Colonnade (Colonnade,Headed,Headless)
import Data.Text (Text)
import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.String (IsString(..))
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Colonnade.Encode as Encode
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as TBuilder

-- $build
--
-- The 'Cell' type is used to build a 'Colonnade' that 
-- has 'Html' content inside table cells and may optionally
-- have attributes added to the @<td>@ or @<th>@ elements
-- that wrap this HTML content.

-- | The attributes that will be applied to a @<td>@ and
--   the HTML content that will go inside it.
data Cell = Cell
  { cellAttributes :: !Attribute
  , cellHtml :: !Html
  }

instance IsString Cell where
  fromString = stringCell

instance Monoid Cell where
  mempty = Cell mempty mempty
  mappend (Cell a1 c1) (Cell a2 c2) = Cell (mappend a1 a2) (mappend c1 c2)

-- | Create a 'Cell' from a 'Widget'
htmlCell :: Html -> Cell
htmlCell = Cell mempty

-- | Create a 'Cell' from a 'String'
stringCell :: String -> Cell
stringCell = htmlCell . fromString

-- | Create a 'Cell' from a 'Text'
textCell :: Text -> Cell
textCell = htmlCell . toHtml

-- | Create a 'Cell' from a lazy text
lazyTextCell :: LText.Text -> Cell
lazyTextCell = textCell . LText.toStrict

-- | Create a 'Cell' from a text builder
builderCell :: TBuilder.Builder -> Cell
builderCell = lazyTextCell . TBuilder.toLazyText

encodeTable ::
     (Foldable f, Foldable h)
  => Maybe Attribute -- ^ Attributes of @<thead>@, pass 'Nothing' to omit @<thead>@
  -> Attribute -- ^ Attributes of @<tbody>@ element
  -> (a -> Attribute) -- ^ Attributes of each @<tr>@ element
  -> ((Html -> Html) -> c -> Html) -- ^ Wrap content and convert to 'Html'
  -> Attribute -- ^ Attributes of @<table>@ element
  -> Colonnade h c a -- ^ How to encode data as a row
  -> f a -- ^ Collection of data
  -> Html
encodeTable mtheadAttrs tbodyAttrs trAttrs wrapContent tableAttrs colonnade xs =
  H.table ! tableAttrs $ do
    for_ mtheadAttrs $ \theadAttrs -> do
      H.thead ! theadAttrs $ do
        Encode.headerMonadicGeneral_ colonnade (wrapContent H.th)
    H.tbody ! tbodyAttrs $ do
      forM_ xs $ \x -> do
        H.tr ! trAttrs x $ Encode.rowMonadic_ colonnade (wrapContent H.td) x

encodeHeadedCellTable :: 
     Foldable f
  => Attribute -- ^ Attributes of @<table>@ element
  -> Colonnade Headed Cell a -- ^ How to encode data as columns
  -> f a -- ^ Collection of data
  -> Html
encodeHeadedCellTable = encodeTable
  (Just mempty) mempty (const mempty) htmlFromCell 

encodeHeadlessCellTable :: 
     Foldable f
  => Attribute -- ^ Attributes of @<table>@ element
  -> Colonnade Headless Cell a -- ^ How to encode data as columns
  -> f a -- ^ Collection of data
  -> Html
encodeHeadlessCellTable = encodeTable
  Nothing mempty (const mempty) htmlFromCell 

encodeHeadedHtmlTable :: 
     Foldable f
  => Attribute -- ^ Attributes of @<table>@ element
  -> Colonnade Headed Html a -- ^ How to encode data as columns
  -> f a -- ^ Collection of data
  -> Html
encodeHeadedHtmlTable = encodeTable
  (Just mempty) mempty (const mempty) ($) 

encodeHeadlessHtmlTable :: 
     Foldable f
  => Attribute -- ^ Attributes of @<table>@ element
  -> Colonnade Headless Html a -- ^ How to encode data as columns
  -> f a -- ^ Collection of data
  -> Html
encodeHeadlessHtmlTable = encodeTable
  Nothing mempty (const mempty) ($) 

tableBody :: Foldable f
  => Colonnade h Cell a -- ^ How to encode data as a row
  -> f a -- ^ Rows of data
  -> Html
tableBody enc xs = H.tbody $ do
  forM_ xs $ \x -> do
    H.tr $ Encode.rowMonadic enc (htmlFromCell H.td) x

htmlFromCell :: (Html -> Html) -> Cell -> Html
htmlFromCell f (Cell attr content) = f ! attr $ content

-- $discussion
--
-- In this module, some of the functions for applying a 'Colonnade' to
-- some values to build a table have roughly this type signature:
--
-- > Foldable a => Colonnade Headedness Cell a -> f a -> Html
--
-- The 'Colonnade'\'s content type is 'Cell', but the content
-- type of the result is 'Html'. It may not be immidiately clear why
-- this is useful done. Another strategy, which this library also
-- uses, is to write
-- these functions to take a 'Colonnade' whose content is 'Html':
--
-- > Foldable a => Colonnade Headedness Html a -> f a -> Html
--
-- When the 'Colonnade'\'s content type is 'Html', then the header
-- content is rendered as the child of a @<th>@ and the row
-- content the child of a @<td>@. However, it is not possible
-- to add attributes to these parent elements. To accomodate this
-- situation, it is necessary to introduce 'Cell', which includes
-- the possibility of attributes on the parent node.


