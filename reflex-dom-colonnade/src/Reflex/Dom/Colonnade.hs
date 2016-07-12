{-# LANGUAGE DeriveFunctor #-}

module Reflex.Dom.Colonnade where

import Colonnade.Types
import Control.Monad
import Data.Foldable
import Reflex (Dynamic,Event,switchPromptly,never)
import Reflex.Dynamic (mapDyn)
import Reflex.Dom (MonadWidget)
import Reflex.Dom.Widget.Basic
import Data.Map (Map)
import Data.Semigroup (Semigroup)
import qualified Colonnade.Encoding as Encoding
import qualified Data.Map as Map

cell :: m b -> Cell m b
cell = Cell Map.empty

data Cell m b = Cell
  { cellAttrs :: !(Map String String)
  , cellContents :: !(m b)
  } deriving (Functor)

-- instance Functor (Cell m) where
--   fmap f (a

basic :: (MonadWidget t m, Foldable f)
      => Map String String -- ^ Table element attributes
      -> f a -- ^ Values
      -> Encoding Headed (Cell m ()) a -- ^ Encoding of a value into cells
      -> m ()
basic tableAttrs as encoding = do
  elAttr "table" tableAttrs $ do
    theadBuild encoding
    el "tbody" $ forM_ as $ \a -> do
      el "tr" $ Encoding.runRowMonadic encoding (elFromCell "td") a

elFromCell :: MonadWidget t m => String -> Cell m b -> m b
elFromCell name (Cell attrs contents) = elAttr name attrs contents

theadBuild :: (MonadWidget t m, Monoid b) => Encoding Headed (Cell m b) a -> m b
theadBuild encoding = el "thead" . el "tr"
  $ Encoding.runHeaderMonadic encoding (elFromCell "th")

dynamic :: (MonadWidget t m, Foldable f)
        => Map String String -- ^ Table element attributes
        -> f (Dynamic t a) -- ^ Dynamic values
        -> Encoding Headed (Cell m ()) a -- ^ Encoding of a value into cells
        -> m ()
dynamic tableAttrs as encoding@(Encoding v) = do
  elAttr "table" tableAttrs $ do
    theadBuild encoding
    el "tbody" $ forM_ as $ \a -> do
      el "tr" $ forM_ v $ \(OneEncoding _ encode) -> do
        dynPair <- mapDyn encode a
        dynAttrs <- mapDyn cellAttrs dynPair
        dynContent <- mapDyn cellContents dynPair
        _ <- elDynAttr "td" dynAttrs $ dyn dynContent
        return ()

dynamicEventful :: (MonadWidget t m, Traversable f, Semigroup e)
  => Map String String -- ^ Table element attributes
  -> f (Dynamic t a) -- ^ Dynamic values
  -> Encoding Headed (Cell m (Event t e)) a -- ^ Encoding of a value into cells
  -> m (Event t e)
dynamicEventful tableAttrs as encoding@(Encoding v) = do
  elAttr "table" tableAttrs $ do
    b1 <- theadBuild encoding
    b2 <- el "tbody" $ forM as $ \a -> do
      el "tr" $ forM v $ \(OneEncoding _ encode) -> do
        dynPair <- mapDyn encode a
        dynAttrs <- mapDyn cellAttrs dynPair
        dynContent <- mapDyn cellContents dynPair
        e <- elDynAttr "td" dynAttrs $ dyn dynContent
        -- TODO: This might actually be wrong. Revisit this.
        switchPromptly never e
    return (mappend b1 (mconcat $ toList $ mconcat $ toList b2))

