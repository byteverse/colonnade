module Reflex.Dom.Colonnade where

import Colonnade.Types
import Control.Monad
import Reflex (Dynamic)
import Reflex.Dynamic (mapDyn)
import Reflex.Dom (MonadWidget)
import Reflex.Dom.Widget.Basic
import Data.Map (Map)
import qualified Data.Map as Map

cell :: m () -> Cell m
cell = Cell Map.empty

data Cell m = Cell
  { cellAttrs :: Map String String
  , cellContents :: m ()
  }

basic :: (MonadWidget t m, Foldable f)
      => Map String String -- ^ Table element attributes
      -> f a -- ^ Values
      -> Encoding Headed (Cell m) a -- ^ Encoding of a value into cells
      -> m ()
basic tableAttrs as (Encoding v) = do
  elAttr "table" tableAttrs $ do
    el "thead" $ el "tr" $ forM_ v $ \(Headed (Cell attrs contents),_) ->
      elAttr "th" attrs contents
    el "tbody" $ forM_ as $ \a -> do
      el "tr" $ forM_ v $ \(_,encode) -> do
        let Cell attrs contents = encode a
        elAttr "td" attrs contents

dynamic :: (MonadWidget t m, Foldable f)
        => Map String String -- ^ Table element attributes
        -> f (Dynamic t a) -- ^ Dynamic values
        -> Encoding Headed (Cell m) a -- ^ Encoding of a value into cells
        -> m ()
dynamic tableAttrs as (Encoding v) = do
  elAttr "table" tableAttrs $ do
    el "thead" $ el "tr" $ forM_ v $ \(Headed (Cell attrs contents),_) ->
      elAttr "th" attrs contents
    el "tbody" $ forM_ as $ \a -> do
      el "tr" $ forM_ v $ \(_,encode) -> do
        dynPair <- mapDyn encode a
        dynAttrs <- mapDyn cellAttrs dynPair
        dynContent <- mapDyn cellContents dynPair
        _ <- elDynAttr "td" dynAttrs $ dyn dynContent
        return ()

