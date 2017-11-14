-- | Build HTML tables using @yesod@ and @colonnade@. To learn
--   how to use this module, first read the documentation for @colonnade@,
--   and then read the documentation for @blaze-colonnade@. This library
--   and @blaze-colonnade@ are entirely distinct; neither depends on the
--   other. However, the interfaces they expose are very similar, and
--   the explanations provided counterpart are sufficient to understand
--   this library.
module Yesod.Colonnade
  ( -- * Build
    Cell(..)
  , cell
  , stringCell
  , textCell
  , builderCell
  , anchorCell
  , anchorWidget
    -- * Apply
  , encodeHeadedWidgetTable
  , encodeHeadlessWidgetTable
  , encodeHeadedCellTable
  , encodeHeadlessCellTable
  , encodeDefinitionTable
  , encodeListItems
  ) where

import Yesod.Core
import Yesod.Core.Types (Body(..),GWData(..),WidgetT(..))
import Colonnade (Colonnade,Headed,Headless)
import Data.Text (Text)
import Control.Monad
import Data.Monoid
import Data.String (IsString(..))
import Text.Blaze (Attribute,toValue)
import Data.Foldable
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Html5 as H
import qualified Colonnade.Encode as E
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as TBuilder

-- | The attributes that will be applied to a @<td>@ and
--   the HTML content that will go inside it.
data Cell site = Cell
  { cellAttrs :: !Attribute
  , cellContents :: !(WidgetT site IO ())
  }

instance IsString (Cell site) where
  fromString = stringCell

instance Monoid (Cell site) where
  mempty = Cell mempty mempty
  mappend (Cell a1 c1) (Cell a2 c2) = Cell (mappend a1 a2) (mappend c1 c2)

-- | Create a 'Cell' from a 'Widget'
cell :: WidgetT site IO () -> Cell site
cell = Cell mempty

-- | Create a 'Cell' from a 'String'
stringCell :: String -> Cell site
stringCell = cell . fromString

-- | Create a 'Cell' from a 'Text'
textCell :: Text -> Cell site
textCell = cell . toWidget . toHtml

-- | Create a 'Cell' from a text builder
builderCell :: TBuilder.Builder -> Cell site
builderCell = cell . toWidget . toHtml . LText.toStrict . TBuilder.toLazyText

-- | Create a 'Cell' whose content is hyperlinked by wrapping
--   it in an @\<a\>@.
anchorCell :: 
     (a -> Route site) -- ^ Route that will go in @href@ attribute
  -> (a -> WidgetT site IO ()) -- ^ Content wrapped by @<a>@ tag
  -> a -- ^ Value
  -> Cell site
anchorCell getRoute getContent = cell . anchorWidget getRoute getContent

-- | Create a widget whose content is hyperlinked by wrapping
--   it in an @\<a\>@.
anchorWidget :: 
     (a -> Route site) -- ^ Route that will go in @href@ attribute
  -> (a -> WidgetT site IO ()) -- ^ Content wrapped by @<a>@ tag
  -> a -- ^ Value
  -> WidgetT site IO ()
anchorWidget getRoute getContent a = do
  urlRender <- getUrlRender
  a_ (HA.href (toValue (urlRender (getRoute a)))) (getContent a)

-- | This determines the attributes that are added
--   to the individual @li@s by concatenating the header\'s
--   attributes with the data\'s attributes. 
encodeListItems :: 
     (WidgetT site IO () -> WidgetT site IO ())
     -- ^ Wrapper for items, often @ul@
  -> (WidgetT site IO () -> WidgetT site IO () -> WidgetT site IO ())
     -- ^ Combines header with data
  -> Colonnade Headed a (Cell site)
     -- ^ How to encode data as a row
  -> a
     -- ^ The value to display
  -> WidgetT site IO ()
encodeListItems ulWrap combine enc =
  ulWrap . E.bothMonadic_ enc
    (\(Cell ha hc) (Cell ba bc) ->
      li_ (ha <> ba) (combine hc bc)
    )

-- | A two-column table with the header content displayed in the
--   first column and the data displayed in the second column. Note
--   that the generated HTML table does not have a @thead@.
encodeDefinitionTable ::
     Attribute
     -- ^ Attributes of @table@ element.
  -> Colonnade Headed a (Cell site)
     -- ^ How to encode data as a row
  -> a
     -- ^ The value to display
  -> WidgetT site IO ()
encodeDefinitionTable attrs enc a = table_ attrs $ tbody_ mempty $ 
  E.bothMonadic_ enc
    (\theKey theValue -> tr_ mempty $ do
      widgetFromCell td_ theKey
      widgetFromCell td_ theValue
    ) a

-- | If you are using the bootstrap css framework, then you may want
--   to call this with the first argument as:
--
--   > encodeHeadedCellTable (HA.class_ "table table-striped") ...
encodeHeadedCellTable :: Foldable f
  => Attribute -- ^ Attributes of @table@ element
  -> Colonnade Headed a (Cell site) -- ^ How to encode data as a row
  -> f a -- ^ Rows of data
  -> WidgetT site IO ()
encodeHeadedCellTable = encodeTable
  (E.Headed mempty) mempty (const mempty) widgetFromCell 

encodeHeadlessCellTable :: Foldable f
  => Attribute -- ^ Attributes of @table@ element
  -> Colonnade Headless a (Cell site) -- ^ How to encode data as columns
  -> f a -- ^ Rows of data
  -> WidgetT site IO ()
encodeHeadlessCellTable = encodeTable
  E.Headless mempty (const mempty) widgetFromCell 

encodeHeadedWidgetTable :: Foldable f
  => Attribute -- ^ Attributes of @table@ element
  -> Colonnade Headed a (WidgetT site IO ()) -- ^ How to encode data as columns
  -> f a -- ^ Rows of data
  -> WidgetT site IO ()
encodeHeadedWidgetTable = encodeTable
  (E.Headed mempty) mempty (const mempty) ($ mempty)

encodeHeadlessWidgetTable :: Foldable f
  => Attribute -- ^ Attributes of @\<table\>@ element
  -> Colonnade Headless a (WidgetT site IO ()) -- ^ How to encode data as columns
  -> f a -- ^ Rows of data
  -> WidgetT site IO ()
encodeHeadlessWidgetTable = encodeTable
  E.Headless mempty (const mempty) ($ mempty) 

-- | Encode a table. This handles a very general case and
--   is seldom needed by users. One of the arguments provided is
--   used to add attributes to the generated @\<tr\>@ elements.
encodeTable ::
     (Foldable f, E.Headedness h)
  => h Attribute -- ^ Attributes of @\<thead\>@, pass 'Nothing' to omit @\<thead\>@
  -> Attribute -- ^ Attributes of @\<tbody\>@ element
  -> (a -> Attribute) -- ^ Attributes of each @\<tr\>@ element
  -> ((Attribute -> WidgetT site IO () -> WidgetT site IO ()) -> c -> WidgetT site IO ()) -- ^ Wrap content and convert to 'Html'
  -> Attribute -- ^ Attributes of @\<table\>@ element
  -> Colonnade h a c -- ^ How to encode data as a row
  -> f a -- ^ Collection of data
  -> WidgetT site IO ()
encodeTable theadAttrs tbodyAttrs trAttrs wrapContent tableAttrs colonnade xs =
  table_ tableAttrs $ do
    for_ E.headednessExtract $ \unhead ->
      thead_ (unhead theadAttrs) $ do
        E.headerMonadicGeneral_ colonnade (wrapContent th_)
    tbody_ tbodyAttrs $ do
      forM_ xs $ \x -> do
        tr_ (trAttrs x) (E.rowMonadic_ colonnade (wrapContent td_) x)

widgetFromCell ::
  (Attribute -> WidgetT site IO () -> WidgetT site IO ())
  -> Cell site
  -> WidgetT site IO ()
widgetFromCell f (Cell attrs contents) =
  f attrs contents

tr_,tbody_,thead_,table_,td_,th_,ul_,li_,a_ ::
  Attribute -> WidgetT site IO () -> WidgetT site IO ()

table_ = liftParent H.table
thead_ = liftParent H.thead
tbody_ = liftParent H.tbody
tr_ = liftParent H.tr
td_ = liftParent H.td
th_ = liftParent H.th
ul_ = liftParent H.ul
li_ = liftParent H.li
a_ = liftParent H.a

liftParent :: (Html -> Html) -> Attribute -> WidgetT site IO a -> WidgetT site IO a
liftParent el attrs (WidgetT f) = WidgetT $ \hdata -> do
  (a,gwd) <- f hdata
  let Body bodyFunc = gwdBody gwd
      newBodyFunc render =
        el H.! attrs $ (bodyFunc render)
  return (a,gwd { gwdBody = Body newBodyFunc })




