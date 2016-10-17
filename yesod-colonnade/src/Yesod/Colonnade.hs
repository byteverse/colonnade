{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Yesod.Colonnade
  ( table
  , listItems
  , Cell(..)
  , cell
  , stringCell
  , textCell
  , builderCell
  ) where

import Yesod.Core
import Colonnade.Types
import Data.Text (Text)
import Control.Monad
import Data.String (IsString(..))
import qualified Colonnade.Encoding as Encoding
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as TBuilder

data Cell site = Cell
  { cellAttrs :: ![(Text,Text)]
  , cellContents :: !(WidgetT site IO ())
  }

instance IsString (Cell site) where
  fromString = stringCell

cell :: WidgetT site IO () -> Cell site
cell = Cell []

stringCell :: String -> Cell site
stringCell = cell . fromString

textCell :: Text -> Cell site
textCell = cell . toWidget . toHtml

builderCell :: TBuilder.Builder -> Cell site
builderCell = cell . toWidget . toHtml . LText.toStrict . TBuilder.toLazyText

-- | This determines the attributes that are added
--   to the individual @li@s by concatenating the header\'s
--   attributes with the data\'s attributes.
listItems :: Foldable f
  => (WidgetT site IO () -> WidgetT site IO ())
     -- ^ Wrapper for items, often @ul@
  -> (WidgetT site IO () -> WidgetT site IO () -> WidgetT site IO ())
     -- ^ Combines header with data
  -> Encoding Headed (Cell site) a
     -- ^ How to encode data as a row
  -> f a
     -- ^ Rows of data
  -> WidgetT site IO ()
listItems ulWrap combine enc xs =
  forM_ xs $ ulWrap . Encoding.runBothMonadic_ enc
    (\(Cell ha hc) (Cell ba bc) ->
      li (ha ++ ba) (combine hc bc)
    )

-- | If you are using the bootstrap css framework, then you may want
--   to call this with the first argument as:
--
--   > table [("class","table table-striped")] ...
table :: Foldable f
  => [(Text,Text)] -- ^ Attributes of @table@ element
  -> Encoding Headed (Cell site) a -- ^ How to encode data as a row
  -> f a -- ^ Rows of data
  -> WidgetT site IO ()
table attrs enc xs = tableEl attrs $ do
  thead [] $ Encoding.runHeaderMonadic enc (widgetFromCell th)
  tableBody enc xs

tableHeadless :: Foldable f
  => [(Text,Text)] -- ^ Attributes of @table@ element
  -> Encoding Headless (Cell site) a -- ^ How to encode data as a row
  -> f a -- ^ Rows of data
  -> WidgetT site IO ()
tableHeadless attrs enc xs = tableEl attrs $ tableBody enc xs

tableBody :: Foldable f
  => Encoding h (Cell site) a -- ^ How to encode data as a row
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

tr,tbody,thead,tableEl,td,th,ul,li ::
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

