{-# LANGUAGE DeriveFunctor #-}

module Reflex.Dom.Colonnade
  ( Cell(..)
  , cell
  , basic
  , dynamic
  , dynamicEventful
  ) where

import Colonnade.Types
import Control.Monad
import Data.Maybe
import Data.Foldable
import Reflex (Dynamic,Event,switchPromptly,never,leftmost)
import Reflex.Dynamic (mapDyn)
import Reflex.Dom (MonadWidget)
import Reflex.Dom.Widget.Basic
import Data.Map (Map)
import Data.Semigroup (Semigroup)
import qualified Data.Vector as Vector
import qualified Colonnade.Encoding as Encoding
import qualified Data.Map as Map

cell :: m b -> Cell m b
cell = Cell Map.empty

-- data NewCell b = NewCell
--   { newCellAttrs :: !(Map String String)
--   , newCellContents :: !b
--   } deriving (Functor)

data Cell m b = Cell
  { cellAttrs :: !(Map String String)
  , cellContents :: !(m b)
  } deriving (Functor)

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

interRowContent :: (MonadWidget t m, Foldable f)
  => String
  -> String
  -> f a
  -> Encoding Headed (Cell m (Event t (Maybe (m ())))) a
  -> m ()
interRowContent tableClass tdExtraClass as encoding@(Encoding v) = do
  let vlen = Vector.length v
  elAttr "table" (Map.singleton "class" tableClass) $ do
    -- Discarding this result is technically the wrong thing
    -- to do, but I cannot imagine why anyone would want to
    -- drop down content under the heading.
    _ <- theadBuild_ encoding
    el "tbody" $ forM_ as $ \a -> do
      e' <- el "tr" $ do
        e <- Encoding.runRowMonadicWith never const encoding (elFromCell "td") a
        let e' = flip fmap e $ \mwidg -> case mwidg of
              Nothing -> return ()
              Just widg -> el "tr" $ do
                elAttr "td" ( Map.fromList
                    [ ("class",tdExtraClass)
                    , ("colspan",show vlen)
                    ]
                  ) widg
        return e'
      widgetHold (return ()) e'

elFromCell :: MonadWidget t m => String -> Cell m b -> m b
elFromCell name (Cell attrs contents) = elAttr name attrs contents

theadBuild :: (MonadWidget t m, Monoid b) => Encoding Headed (Cell m b) a -> m b
theadBuild encoding = el "thead" . el "tr"
  $ Encoding.runHeaderMonadic encoding (elFromCell "th")

theadBuild_ :: (MonadWidget t m) => Encoding Headed (Cell m b) a -> m ()
theadBuild_ encoding = el "thead" . el "tr"
  $ Encoding.runHeaderMonadic_  encoding (elFromCell "th")

dynamic :: (MonadWidget t m, Foldable f)
        => Map String String -- ^ Table element attributes
        -> f (Dynamic t a) -- ^ Dynamic values
        -> Encoding Headed (Cell m ()) a -- ^ Encoding of a value into cells
        -> m ()
dynamic tableAttrs as encoding@(Encoding v) = do
  elAttr "table" tableAttrs $ do
    b1 <- theadBuild encoding
    b2 <- el "tbody" $ forM_ as $ \a -> do
      el "tr" $ forM_ v $ \(OneEncoding _ encode) -> do
        dynPair <- mapDyn encode a
        dynAttrs <- mapDyn cellAttrs dynPair
        dynContent <- mapDyn cellContents dynPair
        elDynAttr "td" dynAttrs $ dyn dynContent
    return (mappend b1 b2)

dynamicEventful :: (MonadWidget t m, Foldable f, Semigroup e)
  => Map String String -- ^ Table element attributes
  -> f (Dynamic t a) -- ^ Dynamic values
  -> Encoding Headed (Cell m (Event t e)) a -- ^ Encoding of a value into cells
  -> m (Event t e)
dynamicEventful tableAttrs as encoding@(Encoding v) = do
  elAttr "table" tableAttrs $ do
    b1 <- theadBuild encoding
    b2 <- el "tbody" $ flip foldMapM as $ \a -> do
      el "tr" $ flip foldMapM v $ \(OneEncoding _ encode) -> do
        dynPair <- mapDyn encode a
        dynAttrs <- mapDyn cellAttrs dynPair
        dynContent <- mapDyn cellContents dynPair
        e <- elDynAttr "td" dynAttrs $ dyn dynContent
        -- TODO: This might actually be wrong. Revisit this.
        switchPromptly never e
    return (mappend b1 b2)

foldMapM :: (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldMapM f = foldrM (\a b -> fmap (flip mappend b) (f a)) mempty

foldAlternativeM :: (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldAlternativeM f = foldrM (\a b -> fmap (flip mappend b) (f a)) mempty

-- dynamicEventfulWith :: (MonadWidget t m, Foldable f, Semigroup e, Monoid b)
--   => (e -> b)
--   -> Map String String -- ^ Table element attributes
--   -> f (Dynamic t a) -- ^ Dynamic values
--   -> Encoding Headed (Cell m (Event t e)) a -- ^ Encoding of a value into cells
--   -> m (Event t e)
-- dynamicEventfulWith f tableAttrs as encoding@(Encoding v) = do
--   elAttr "table" tableAttrs $ do
--     b1 <- theadBuild encoding
--     b2 <- el "tbody" $ flip foldMapM as $ \a -> do
--       el "tr" $ flip foldMapM v $ \(OneEncoding _ encode) -> do
--         dynPair <- mapDyn encode a
--         dynAttrs <- mapDyn cellAttrs dynPair
--         dynContent <- mapDyn cellContents dynPair
--         e <- elDynAttr "td" dynAttrs $ dyn dynContent
--         flattenedEvent <- switchPromptly never e
--         return (f flattenedEvent)
--     return (mappend b1 b2)
--
-- dynamicEventfulMany :: (MonadWidget t m, Foldable f, Alternative g)
--   => Map String String -- ^ Table element attributes
--   -> f (Dynamic t a) -- ^ Dynamic values
--   -> Encoding Headed (NewCell (g (Compose m (Event t)))) a -- ^ Encoding of a value into cells
--   -> m (g (Event t e))
-- dynamicEventfulMany tableAttrs as encoding@(Encoding v) = do
--   elAttr "table" tableAttrs $ do
--     -- b1 <- theadBuild encoding
--     b2 <- el "tbody" $ flip foldMapM as $ \a -> do
--       el "tr" $ flip foldMapM v $ \(OneEncoding _ encode) -> do
--         dynPair <- mapDyn encode a
--         dynAttrs <- mapDyn cellAttrs dynPair
--         dynContent <- mapDyn cellContents dynPair
--         e <- elDynAttr "td" dynAttrs $ dyn dynContent
--         switchPromptly never e
--     return (mappend b1 b2)

-- data Update f = UpdateName (f Text) | UpdateAge (f Int) | ...

