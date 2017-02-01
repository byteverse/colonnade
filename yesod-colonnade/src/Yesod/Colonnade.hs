{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Yesod.Colonnade
  ( -- * Build Encoding
    Cell(..)
  , cell
  , stringCell
  , textCell
  , builderCell
  , anchorCell
    -- * Apply Encoding
  , table
  , tableHeadless
  , definitionTable
  , listItems
  ) where

import Yesod.Core
import Colonnade.Types (Colonnade,Headed,Headless)
import Data.Text (Text)
import Control.Monad
import Data.Monoid
import Data.String (IsString(..))
import qualified Colonnade.Encoding as Encoding
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as TBuilder

-- | The attributes that will be applied to a @<td>@ and
--   the HTML content that will go inside it.
data Cell site = Cell
  { cellAttrs :: ![(Text,Text)]
  , cellContents :: !(WidgetT site IO ())
  }

instance IsString (Cell site) where
  fromString = stringCell

instance Monoid (Cell site) where
  mempty = Cell [] mempty
  mappend (Cell a1 c1) (Cell a2 c2) = Cell (mappend a1 a2) (mappend c1 c2)

-- | Create a 'Cell' from a 'Widget'
cell :: WidgetT site IO () -> Cell site
cell = Cell []

-- | Create a 'Cell' from a 'String'
stringCell :: String -> Cell site
stringCell = cell . fromString

-- | Create a 'Cell' from a 'Text'
textCell :: Text -> Cell site
textCell = cell . toWidget . toHtml

-- | Create a 'Cell' from a text builder
builderCell :: TBuilder.Builder -> Cell site
builderCell = cell . toWidget . toHtml . LText.toStrict . TBuilder.toLazyText

-- | Creata a 'Cell' whose content is hyperlinked by wrapping
--   it in an @<a>@.
anchorCell :: 
     (a -> Route site) -- ^ Route that will go in @href@
  -> (a -> WidgetT site IO ()) -- ^ Content wrapped by @<a>@
  -> a -- ^ Value
  -> Cell site
anchorCell getRoute getContent a = cell $ do
  urlRender <- getUrlRender
  aTag [(Text.pack "href",urlRender (getRoute a))] (getContent a)

-- | This determines the attributes that are added
--   to the individual @li@s by concatenating the header\'s
--   attributes with the data\'s attributes. 
listItems :: 
     (WidgetT site IO () -> WidgetT site IO ())
     -- ^ Wrapper for items, often @ul@
  -> (WidgetT site IO () -> WidgetT site IO () -> WidgetT site IO ())
     -- ^ Combines header with data
  -> Colonnade Headed (Cell site) a
     -- ^ How to encode data as a row
  -> a
     -- ^ The value to display
  -> WidgetT site IO ()
listItems ulWrap combine enc =
  ulWrap . Encoding.runBothMonadic_ enc
    (\(Cell ha hc) (Cell ba bc) ->
      li (ha ++ ba) (combine hc bc)
    )

-- | A two-column table with the header content displayed in the
--   first column and the data displayed in the second column. Note
--   that the generated HTML table does not have a @thead@.
definitionTable ::
     [(Text,Text)]
     -- ^ Attributes of @table@ element.
  -> Colonnade Headed (Cell site) a
     -- ^ How to encode data as a row
  -> a
     -- ^ The value to display
  -> WidgetT site IO ()
definitionTable attrs enc a = tableEl attrs $ tbody [] $ 
  Encoding.runBothMonadic_ enc
    (\theKey theValue -> tr [] $ do
      widgetFromCell td theKey
      widgetFromCell td theValue
    ) a

-- | If you are using the bootstrap css framework, then you may want
--   to call this with the first argument as:
--
--   > table [("class","table table-striped")] ...
table :: Foldable f
  => [(Text,Text)] -- ^ Attributes of @table@ element
  -> Colonnade Headed (Cell site) a -- ^ How to encode data as a row
  -> f a -- ^ Rows of data
  -> WidgetT site IO ()
table attrs enc xs = tableEl attrs $ do
  thead [] $ Encoding.runHeaderMonadic enc (widgetFromCell th)
  tableBody enc xs

tableHeadless :: Foldable f
  => [(Text,Text)] -- ^ Attributes of @table@ element
  -> Colonnade Headless (Cell site) a -- ^ How to encode data as a row
  -> f a -- ^ Rows of data
  -> WidgetT site IO ()
tableHeadless attrs enc xs = tableEl attrs $ tableBody enc xs

tableBody :: Foldable f
  => Colonnade h (Cell site) a -- ^ How to encode data as a row
  -> f a -- ^ Rows of data
  -> WidgetT site IO ()
tableBody enc xs = tbody [] $ do
  forM_ xs $ \x -> do
    tr [] $ Encoding.runRowMonadic enc (widgetFromCell td) x

widgetFromCell ::
  ([(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ())
  -> Cell site
  -> WidgetT site IO ()
widgetFromCell f (Cell attrs contents) =
  f attrs contents

tr,tbody,thead,tableEl,td,th,ul,li,aTag ::
  [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
tableEl str b = [whamlet|
  <table *{str}>^{b}
|]
thead str b = [whamlet|
  <thead *{str}>^{b}
|]
tbody str b = [whamlet|
  <tbody *{str}>^{b}
|]
tr str b = [whamlet|
  <tr *{str}>^{b}
|]
th str b = [whamlet|
  <th *{str}>^{b}
|]
td str b = [whamlet|
  <td *{str}>^{b}
|]
ul str b = [whamlet|
  <ul *{str}>^{b}
|]
li str b = [whamlet|
  <li *{str}>^{b}
|]
aTag str b = [whamlet|
  <a *{str}>^{b}
|]

