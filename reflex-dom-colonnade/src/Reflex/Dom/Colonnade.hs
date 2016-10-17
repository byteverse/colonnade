{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Dom.Colonnade
  (
  -- * Types
    Cell(..)
  -- * Table Encoders
  , basic
  , dynamic
  , dynamicEventful
  , expandable
  , listItems
    -- * Cell Functions
  , cell
  , stringCell
  , textCell
  , builderCell
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
import Data.Text (Text)
import Data.String (IsString(..))
import qualified Data.Vector as Vector
import qualified Colonnade.Encoding as Encoding
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as TBuilder

-- | Convenience function for creating a 'Cell' representing
--   a @td@ or @th@ with no attributes.
cell :: m b -> Cell m b
cell = Cell Map.empty

stringCell :: MonadWidget t m => String -> Cell m ()
stringCell = cell . text

textCell :: MonadWidget t m => Text -> Cell m ()
textCell = cell . text . Text.unpack

builderCell :: MonadWidget t m => TBuilder.Builder -> Cell m ()
builderCell = textCell . LText.toStrict . TBuilder.toLazyText

-- data NewCell b = NewCell
--   { newCellAttrs :: !(Map String String)
--   , newCellContents :: !b
--   } deriving (Functor)

data Cell m b = Cell
  { cellAttrs :: !(Map String String)
  , cellContents :: !(m b)
  } deriving (Functor)

-- | This instance is requires @UndecidableInstances@ and is kind of
--   bad, but @reflex@ already abusing type classes so much that it
--   doesn\'t seem too terrible to add this to the mix.
instance (MonadWidget t m, a ~ ()) => IsString (Cell m a) where
  fromString = stringCell

-- | This determines the attributes that are added
--   to the individual @li@s by concatenating the header\'s
--   attributes with the data\'s attributes.
listItems :: (Foldable f, MonadWidget t m)
  => (m () -> m ())
     -- ^ Wrapper for items, often @ul@
  -> (m () -> m () -> m ())
     -- ^ Combines header with data
  -> Encoding Headed (Cell m ()) a
     -- ^ How to encode data as a row
  -> f a
     -- ^ Rows of data
  -> m ()
listItems ulWrap combine enc xs =
  forM_ xs $ ulWrap . Encoding.runBothMonadic_ enc
    (\(Cell ha hc) (Cell ba bc) ->
      -- Consider doing something better than union for
      -- combining the two maps. For example, what if they
      -- both have a class.
      elAttr "li" (Map.union ha ba) (combine hc bc)
    )

-- | A static table
basic :: (MonadWidget t m, Foldable f)
      => Map String String -- ^ Table element attributes
      -> Encoding Headed (Cell m ()) a -- ^ Encoding of a value into cells
      -> f a -- ^ Values
      -> m ()
basic tableAttrs encoding as = do
  elAttr "table" tableAttrs $ do
    theadBuild encoding
    el "tbody" $ forM_ as $ \a -> do
      el "tr" $ Encoding.runRowMonadic encoding (elFromCell "td") a

-- | Table with cells that can create expanded content
--   between the rows.
expandable :: (MonadWidget t m, Foldable f)
  => String -- ^ Table class
  -> String -- ^ Class of expanded table rows
  -> f a -- ^ Values
  -> Encoding Headed (Cell m (Event t (Maybe (m ())))) a
     -- ^ Encoding into cells with events that can fire to create additional content under the row
  -> m ()
expandable tableClass tdExtraClass as encoding@(Encoding v) = do
  let vlen = Vector.length v
  elAttr "table" (Map.singleton "class" tableClass) $ do
    -- Discarding this result is technically the wrong thing
    -- to do, but I cannot imagine why anyone would want to
    -- drop down content under the heading.
    _ <- theadBuild_ encoding
    el "tbody" $ forM_ as $ \a -> do
      e' <- el "tr" $ do
        elist <- Encoding.runRowMonadicWith [] (++) encoding (fmap (\a -> [a]) . elFromCell "td") a
        let e = leftmost elist
            e' = flip fmap e $ \mwidg -> case mwidg of
              Nothing -> return ()
              Just widg -> el "tr" $ do
                elAttr "td" ( Map.fromList
                    [ ("class",tdExtraClass)
                    , ("colspan",show vlen)
                    ]
                  ) widg
        return e'
      widgetHold (return ()) e'

-- TODO: figure out how to write this. It will need to reset
-- the interrow content whenever its corresponding row changes.
--
-- dynamicExpandable :: (MonadWidget t m, Foldable f)
--   => String
--   -> String
--   -> f (Dynamic t a)
--   -> Encoding Headed (Cell m (Event t (Maybe (m ())))) a
--   -> m ()

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
    b2 <- el "tbody" $ flip foldlMapM as $ \a -> do
      el "tr" $ flip foldlMapM v $ \(OneEncoding _ encode) -> do
        dynPair <- mapDyn encode a
        dynAttrs <- mapDyn cellAttrs dynPair
        dynContent <- mapDyn cellContents dynPair
        e <- elDynAttr "td" dynAttrs $ dyn dynContent
        -- TODO: This might actually be wrong. Revisit this.
        switchPromptly never e
    return (mappend b1 b2)

-- foldMapM :: (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
-- foldMapM f = foldlM (\b a -> fmap (flip mappend b) (f a)) mempty

foldlMapM :: (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldlMapM f = foldlM (\b a -> fmap (mappend b) (f a)) mempty

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

