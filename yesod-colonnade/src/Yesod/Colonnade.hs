{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Yesod.Colonnade
  ( table
  ) where

import Yesod.Core
import Colonnade.Types
import Data.Text (Text)
import Control.Monad
import qualified Colonnade.Encoding as Encoding

data Cell site = Cell
  { cellAttrs :: ![(Text,Text)]
  , cellContents :: !(WidgetT site IO ())
  }

cell :: WidgetT site IO () -> Cell site
cell = Cell []

textCell :: Text -> Cell site
textCell = cell . toWidget . toHtml

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

tr,tbody,thead,tableEl,td,th :: [(Text,Text)] -> WidgetT site IO () -> WidgetT site IO ()
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

