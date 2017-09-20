{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Reflex.Dom.Colonnade
  (
  -- * Types
    Cell(..)
  , Resizable(..)
  -- * Table Encoders
  , basic
  , static
  , staticTableless
  , capped
  , cappedResizable
  , cappedResizableTableless
  , cappedTraversing
  , dynamic
  , dynamicCapped
  , expandable
  , sectioned
    -- * Cell Functions
  , cell
  , charCell
  , stringCell
  , textCell
  , lazyTextCell
  , builderCell
  ) where

import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Foldable (Foldable(..),for_,forM_)
import Data.Traversable (for)
import Data.Semigroup (Semigroup(..))
import Control.Applicative (liftA2)
import Reflex.Dom
import Colonnade (Colonnade,Headed,Fascia,Cornice)
import Data.Monoid (Sum(..))
import qualified Colonnade as C
import qualified Colonnade.Encode as E

data Cell t m b = Cell
  { cellAttrs    :: !(Dynamic t (M.Map T.Text T.Text))
  , cellContents :: !(m b)
  } deriving (Functor)

-- | In practice, this size will only ever be set to zero
--   or one.
data Resizable t h b = Resizable
  { resizableSize :: !(Dynamic t Int)
  , resizableContent :: !(h b)
  }

elFromCell :: (DomBuilder t m, PostBuild t m) => T.Text -> Cell t m b -> m b
elFromCell e (Cell attr m) = elDynAttr e attr m

-- | Convenience function for creating a 'Cell' representing
--   a @td@ or @th@ with no attributes.
cell :: Reflex t => m b -> Cell t m b
cell = Cell (pure M.empty)

charCell :: DomBuilder t m => Char -> Cell t m ()
charCell = textCell . T.singleton

stringCell :: DomBuilder t m => String -> Cell t m ()
stringCell = cell . text . T.pack

textCell :: DomBuilder t m => T.Text -> Cell t m ()
textCell = cell . text

lazyTextCell :: DomBuilder t m => LT.Text -> Cell t m ()
lazyTextCell = textCell . LT.toStrict

builderCell :: DomBuilder t m => LT.Builder -> Cell t m ()
builderCell = textCell . LT.toStrict . LT.toLazyText

-- | This instance is requires @UndecidableInstances@ and is kind of
--   bad, but @reflex@ already abusing type classes so much that it
--   doesn\'t seem too terrible to add this to the mix.
instance (DomBuilder t m, a ~ ()) => IsString (Cell t m a) where
  fromString = stringCell

newtype WrappedApplicative m a = WrappedApplicative
  { unWrappedApplicative :: m a }
  deriving (Functor,Applicative,Monad)

instance (Semigroup a, Applicative m) => Semigroup (WrappedApplicative m a) where
  (WrappedApplicative m1) <> (WrappedApplicative m2) = WrappedApplicative (liftA2 (<>) m1 m2)

instance (Monoid a, Applicative m) => Monoid (WrappedApplicative m a) where
  mempty = WrappedApplicative (pure mempty)
  mappend (WrappedApplicative m1) (WrappedApplicative m2) = WrappedApplicative (liftA2 mappend m1 m2)

basic ::
  (DomBuilder t m, PostBuild t m, Foldable f)
  => M.Map T.Text T.Text -- ^ @\<table\>@ tag attributes
  -> Colonnade Headed a (Cell t m ()) -- ^ Data encoding strategy
  -> f a -- ^ Collection of data
  -> m ()
basic tableAttrs = static tableAttrs (Just (M.empty,M.empty)) mempty (const mempty)

body :: (DomBuilder t m, PostBuild t m, Foldable f, Monoid e)
  => M.Map T.Text T.Text
  -> (a -> Dynamic t (M.Map T.Text T.Text))
  -> Colonnade h a (Cell t m e)
  -> f a
  -> m e
body bodyAttrs trAttrs colonnade collection =
  elAttr "tbody" bodyAttrs (bodyRows trAttrs colonnade collection)

bodyRows :: (DomBuilder t m, PostBuild t m, Foldable f, Monoid e)
  => (a -> Dynamic t (M.Map T.Text T.Text))
  -> Colonnade p a (Cell t m e)
  -> f a
  -> m e
bodyRows trAttrs colonnade collection =
  unWrappedApplicative . flip foldMap collection $ \a ->
    WrappedApplicative .
    elDynAttr "tr" (trAttrs a) .
    unWrappedApplicative $
    E.rowMonoidal colonnade (WrappedApplicative . elFromCell "td") a

bodyResizable :: (DomBuilder t m, PostBuild t m, Foldable f, Monoid e)
  => Map Text Text
  -> (a -> Map Text Text)
  -> Colonnade (Resizable t h) a (Cell t m e)
  -> f a
  -> m e
bodyResizable bodyAttrs trAttrs colonnade collection = elAttr "tbody" bodyAttrs $ do
  unWrappedApplicative . flip foldMap collection $ \a -> WrappedApplicative
    $ elAttr "tr" (trAttrs a)
    $ unWrappedApplicative
    $ E.rowMonoidalHeader colonnade (\(Resizable dynSize _) (Cell cattr content) -> 
        WrappedApplicative (elDynAttr "td" (zipDynWith setColspanOrHide dynSize cattr) content)) a

setColspanOrHide :: Int -> Map Text Text -> Map Text Text
setColspanOrHide i m
  | i < 1 = M.insertWith T.append "style" "display:none;" m
  | otherwise = M.insert "colspan" (T.pack (show i)) m

static ::
  (DomBuilder t m, PostBuild t m, Foldable f, Foldable h, Monoid e)
  => M.Map T.Text T.Text -- ^ @\<table\>@ tag attributes
  -> Maybe (M.Map T.Text T.Text, M.Map T.Text T.Text)
  -- ^ Attributes of @\<thead\>@ and its @\<tr\>@, pass 'Nothing' to omit @\<thead\>@
  -> M.Map T.Text T.Text -- ^ @\<tbody\>@ tag attributes
  -> (a -> M.Map T.Text T.Text) -- ^ @\<tr\>@ tag attributes
  -> Colonnade h a (Cell t m e) -- ^ Data encoding strategy
  -> f a -- ^ Collection of data
  -> m e
static tableAttrs mheadAttrs bodyAttrs trAttrs colonnade collection =
  elAttr "table" tableAttrs $ do
    for_ mheadAttrs $ \(headAttrs,headTrAttrs) ->
      elAttr "thead" headAttrs . elAttr "tr" headTrAttrs $
        E.headerMonadicGeneral_ colonnade (elFromCell "th")
    body bodyAttrs (pure . trAttrs) colonnade collection

staticTableless ::
  (DomBuilder t m, PostBuild t m, Foldable f, Foldable h, Monoid e)
  => Maybe (M.Map T.Text T.Text, M.Map T.Text T.Text)
  -- ^ Attributes of @\<thead\>@ and its @\<tr\>@, pass 'Nothing' to omit @\<thead\>@
  -> M.Map T.Text T.Text -- ^ @\<tbody\>@ tag attributes
  -> (a -> Dynamic t (M.Map T.Text T.Text)) -- ^ @\<tr\>@ tag attributes
  -> Colonnade h a (Cell t m e) -- ^ Data encoding strategy
  -> f a -- ^ Collection of data
  -> m e
staticTableless mheadAttrs bodyAttrs trAttrs colonnade collection = do
  for_ mheadAttrs $ \(headAttrs,headTrAttrs) ->
    elAttr "thead" headAttrs . elAttr "tr" headTrAttrs $
      E.headerMonadicGeneral_ colonnade (elFromCell "th")
  body bodyAttrs trAttrs colonnade collection

-- | A table dividing into sections by @\<td\>@ elements that
--   take up entire rows.
sectioned :: 
  (DomBuilder t m, PostBuild t m, Foldable f, Foldable h, Foldable g)
  => M.Map T.Text T.Text -- ^ @\<table\>@ tag attributes
  -> Maybe (M.Map T.Text T.Text, M.Map T.Text T.Text)
  -- ^ Attributes of @\<thead\>@ and its @\<tr\>@, pass 'Nothing' to omit @\<thead\>@
  -> M.Map T.Text T.Text -- ^ @\<tbody\>@ tag attributes
  -> (a -> M.Map T.Text T.Text) -- ^ @\<tr\>@ tag attributes for data rows
  -> (b -> Cell t m ()) -- ^ Section divider encoding strategy
  -> Colonnade h a (Cell t m ()) -- ^ Data encoding strategy
  -> f (b, g a) -- ^ Collection of data
  -> m ()
sectioned tableAttrs mheadAttrs bodyAttrs trAttrs dividerContent colonnade@(E.Colonnade v) collection = do
  let vlen = V.length v
  elAttr "table" tableAttrs $ do
    for_ mheadAttrs $ \(headAttrs,headTrAttrs) ->
      elAttr "thead" headAttrs . elAttr "tr" headTrAttrs $
        E.headerMonadicGeneral_ colonnade (elFromCell "th")
    elAttr "tbody" bodyAttrs $ forM_ collection $ \(b,as) -> do
      let Cell attrsB contentsB = dividerContent b
      elAttr "tr" M.empty $ do
        elDynAttr "td" (M.insert "colspan" (T.pack (show vlen)) <$> attrsB) contentsB
      bodyRows (pure . trAttrs) colonnade as

encodeCorniceHead ::
     (DomBuilder t m, PostBuild t m, Monoid e)
  => M.Map T.Text T.Text
  -> Fascia p (M.Map T.Text T.Text)
  -> E.AnnotatedCornice (Maybe Int) p a (Cell t m e)
  -> m e
encodeCorniceHead headAttrs fascia annCornice =
  elAttr "thead" headAttrs (unWrappedApplicative thead)
  where thead = E.headersMonoidal (Just (fascia, addAttr)) [(th,id)] annCornice
        th size (Cell attrs contents) = WrappedApplicative (elDynAttr "th" (fmap addColspan attrs) contents)
          where addColspan = M.insert "colspan" (T.pack (show size))
        addAttr attrs = WrappedApplicative . elAttr "tr" attrs . unWrappedApplicative

encodeCorniceResizableHead :: forall t m e p a.
     (DomBuilder t m, PostBuild t m, Monoid e)
  => M.Map T.Text T.Text
  -> Fascia p (M.Map T.Text T.Text)
  -> E.AnnotatedCornice (Dynamic t Int) p a (Cell t m e)
  -> m e
encodeCorniceResizableHead headAttrs fascia annCornice =
  elAttr "thead" headAttrs (unWrappedApplicative thead)
  where 
  thead :: WrappedApplicative m e
  thead = E.headersMonoidal (Just (fascia, addAttr)) [(th,id)] annCornice
  th :: Dynamic t Int -> Cell t m e -> WrappedApplicative m e
  th size (Cell attrs contents) = WrappedApplicative (elDynAttr "th" (zipDynWith setColspanOrHide size attrs) contents)
  addAttr :: Map Text Text -> WrappedApplicative m b -> WrappedApplicative m b
  addAttr attrs = WrappedApplicative . elAttr "tr" attrs . unWrappedApplicative

capped ::
     (DomBuilder t m, PostBuild t m, MonadHold t m, Foldable f, Monoid e)
  => M.Map T.Text T.Text -- ^ @\<table\>@ tag attributes
  -> M.Map T.Text T.Text -- ^ @\<thead\>@ tag attributes
  -> M.Map T.Text T.Text -- ^ @\<tbody\>@ tag attributes
  -> (a -> M.Map T.Text T.Text) -- ^ @\<tr\>@ tag attributes
  -> Fascia p (M.Map T.Text T.Text) -- ^ Attributes for @\<tr\>@ elements in the @\<thead\>@
  -> Cornice Headed p a (Cell t m e) -- ^ Data encoding strategy
  -> f a -- ^ Collection of data
  -> m e
capped tableAttrs headAttrs bodyAttrs trAttrs fascia cornice collection =
  elAttr "table" tableAttrs $ do
    h <- encodeCorniceHead headAttrs fascia (E.annotate cornice)
    b <- body bodyAttrs (pure . trAttrs) (E.discard cornice) collection
    return (h `mappend` b)

-- | This is useful when you want to be able to toggle the visibility
--   of columns after the table has been built. In additon to the
--   usual monoidal result, the return value also includes a 'Dynamic'
--   that gives the current number of visible columns. This is seldom
--   useful, but it can be helpful if the table footer needs to be
--   given a @colspan@ that matches the number of visible columns.
cappedResizable :: 
     (DomBuilder t m, PostBuild t m, MonadHold t m, Foldable f, Monoid e)
  => Map Text Text -- ^ @\<table\>@ tag attributes
  -> Map Text Text -- ^ @\<thead\>@ tag attributes
  -> Map Text Text -- ^ @\<tbody\>@ tag attributes
  -> m c -- ^ Content beneath @\<tbody\>@. Should either be empty or a @\<tfoot\>@.
  -> (a -> Map Text Text) -- ^ @\<tr\>@ tag attributes
  -> Fascia p (Map Text Text) -- ^ Attributes for @\<tr\>@ elements in the @\<thead\>@
  -> Cornice (Resizable t Headed) p a (Cell t m e) -- ^ Data encoding strategy
  -> f a -- ^ Collection of data
  -> m (e, c, Dynamic t Int)
cappedResizable tableAttrs headAttrs bodyAttrs beneathBody trAttrs fascia cornice collection = do
  elAttr "table" tableAttrs $ do
    let annCornice = dynamicAnnotate cornice
    h <- encodeCorniceResizableHead headAttrs fascia annCornice
    b <- bodyResizable bodyAttrs trAttrs (E.discard cornice) collection
    c <- beneathBody
    return (h `mappend` b, c, E.size annCornice)

-- | Same as 'cappedResizable' but without the @\<table\>@ wrapping it.
--   Also, it does not take extra content to go beneath the @\<tbody\>@.
cappedResizableTableless :: 
     (DomBuilder t m, PostBuild t m, MonadHold t m, Foldable f, Monoid e)
  => Map Text Text -- ^ @\<thead\>@ tag attributes
  -> Map Text Text -- ^ @\<tbody\>@ tag attributes
  -> (a -> Map Text Text) -- ^ @\<tr\>@ tag attributes
  -> Fascia p (Map Text Text) -- ^ Attributes for @\<tr\>@ elements in the @\<thead\>@
  -> Cornice (Resizable t Headed) p a (Cell t m e) -- ^ Data encoding strategy
  -> f a -- ^ Collection of data
  -> m (e, Dynamic t Int)
cappedResizableTableless headAttrs bodyAttrs trAttrs fascia cornice collection = do
  let annCornice = dynamicAnnotate cornice
  h <- encodeCorniceResizableHead headAttrs fascia annCornice
  b <- bodyResizable bodyAttrs trAttrs (E.discard cornice) collection
  return (h `mappend` b, E.size annCornice)

dynamicAnnotate :: Reflex t
  => Cornice (Resizable t Headed) p a c
  -> E.AnnotatedCornice (Dynamic t Int) p a c
dynamicAnnotate = go where
  go :: forall t p a c. Reflex t
    => Cornice (Resizable t Headed) p a c 
    -> E.AnnotatedCornice (Dynamic t Int) p a c
  go (E.CorniceBase c@(E.Colonnade cs)) =
    let parentSz :: Dynamic t (Sum Int)
        parentSz = foldMap (\(E.OneColonnade (Resizable sz _) _) -> (coerceDynamic sz :: Dynamic t (Sum Int))) cs
     in E.AnnotatedCorniceBase (coerceDynamic parentSz) (C.mapHeadedness (\(Resizable dynSize (E.Headed content)) -> E.Sized dynSize (E.Headed content)) c)
  go (E.CorniceCap children) =
    let annChildren = fmap (mapOneCorniceBody go) children
        parentSz :: Dynamic t (Sum Int)
        parentSz = foldMap (\(E.OneCornice _ theBody) -> (coerceDynamic (E.size theBody) :: Dynamic t (Sum Int))) annChildren
     in E.AnnotatedCorniceCap (coerceDynamic parentSz) annChildren

mapOneCorniceBody :: (forall p' a' c'. k p' a' c' -> j p' a' c') -> E.OneCornice k p a c -> E.OneCornice j p a c
mapOneCorniceBody f (E.OneCornice h b) = E.OneCornice h (f b)

bodyTraversing :: (DomBuilder t m, PostBuild t m, Traversable f, Monoid e)
  => M.Map T.Text T.Text
  -> (a -> M.Map T.Text T.Text)
  -> Colonnade p a (Cell t m e)
  -> f a
  -> m (f e)
bodyTraversing bodyAttrs trAttrs colonnade collection =
  elAttr "tbody" bodyAttrs . for collection $ \a ->
    elAttr "tr" (trAttrs a) .
    unWrappedApplicative $
    E.rowMonoidal colonnade (WrappedApplicative . elFromCell "td") a

cappedTraversing ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, Traversable f, Monoid e)
  => M.Map T.Text T.Text -- ^ @\<table\>@ tag attributes
  -> M.Map T.Text T.Text -- ^ @\<thead\>@ tag attributes
  -> M.Map T.Text T.Text -- ^ @\<tbody\>@ tag attributes
  -> (a -> M.Map T.Text T.Text) -- ^ @\<tr\>@ tag attributes
  -> Fascia p (M.Map T.Text T.Text) -- ^ Attributes for @\<tr\>@ elements in the @\<thead\>@
  -> Cornice Headed p a (Cell t m e) -- ^ Data encoding strategy
  -> f a -- ^ Collection of data
  -> m (f e)
cappedTraversing tableAttrs headAttrs bodyAttrs trAttrs fascia cornice collection =
  elAttr "table" tableAttrs $ do
    _ <- encodeCorniceHead headAttrs fascia (E.annotate cornice)
    b <- bodyTraversing bodyAttrs trAttrs (E.discard cornice) collection
    return b

dynamicBody :: (DomBuilder t m, PostBuild t m, Foldable f, Semigroup e, Monoid e)
  => Dynamic t (M.Map T.Text T.Text)
  -> (a -> M.Map T.Text T.Text)
  -> Colonnade p a (Cell t m e)
  -> Dynamic t (f a)
  -> m (Event t e)
dynamicBody bodyAttrs trAttrs colonnade dynCollection =
  elDynAttr "tbody" bodyAttrs . dyn . ffor dynCollection $ \collection ->
    unWrappedApplicative .
    flip foldMap collection $ \a ->
      WrappedApplicative .
      elAttr "tr" (trAttrs a) .
      unWrappedApplicative . E.rowMonoidal colonnade (WrappedApplicative . elFromCell "td") $ a

dynamic ::
  (DomBuilder t m, PostBuild t m, Foldable f, Foldable h, Semigroup e, Monoid e)
  => Dynamic t (M.Map T.Text T.Text) -- ^ @\<table\>@ tag attributes
  -> Maybe (Dynamic t (M.Map T.Text T.Text), Dynamic t (M.Map T.Text T.Text))
  -- ^ Attributes of @\<thead\>@ and its @\<tr\>@, pass 'Nothing' to omit @\<thead\>@
  -> Dynamic t (M.Map T.Text T.Text) -- ^ @\<tbody\>@ tag attributes
  -> (a -> M.Map T.Text T.Text) -- ^ @\<tr\>@ tag attributes
  -> Colonnade h a (Cell t m e) -- ^ Data encoding strategy
  -> Dynamic t (f a) -- ^ Collection of data
  -> m (Event t e)
dynamic tableAttrs mheadAttrs bodyAttrs trAttrs colonnade collection =
  elDynAttr "table" tableAttrs $ do
    for_ mheadAttrs $ \(headAttrs,headTrAttrs) ->
      elDynAttr "thead" headAttrs . elDynAttr "tr" headTrAttrs $
        E.headerMonadicGeneral_ colonnade (elFromCell "th")
    dynamicBody bodyAttrs trAttrs colonnade collection

encodeCorniceHeadDynamic ::
  (DomBuilder t m, PostBuild t m, Monoid e)
  => Dynamic t (M.Map T.Text T.Text)
  -> Fascia p (Dynamic t (M.Map T.Text T.Text))
  -> E.AnnotatedCornice (Maybe Int) p a (Cell t m e)
  -> m e
encodeCorniceHeadDynamic headAttrs fascia annCornice =
  elDynAttr "thead" headAttrs (unWrappedApplicative thead)
  where thead = E.headersMonoidal (Just (fascia, addAttr)) [(th,id)] annCornice
        th size (Cell attrs contents) = WrappedApplicative (elDynAttr "th" (fmap addColspan attrs) contents)
          where addColspan = M.insert "colspan" (T.pack (show size))
        addAttr attrs = WrappedApplicative . elDynAttr "tr" attrs . unWrappedApplicative

dynamicCapped ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, Foldable f, Semigroup e, Monoid e)
  => Dynamic t (M.Map T.Text T.Text) -- ^ @\<table\>@ tag attributes
  -> Dynamic t (M.Map T.Text T.Text) -- ^ @\<thead\>@ tag attributes
  -> Dynamic t (M.Map T.Text T.Text) -- ^ @\<tbody\>@ tag attributes
  -> (a -> M.Map T.Text T.Text) -- ^ @\<tr\>@ tag attributes
  -> Fascia p (Dynamic t (M.Map T.Text T.Text)) -- ^ Attributes for @\<tr\>@ elements in the @\<thead\>@
  -> Cornice Headed p a (Cell t m e) -- ^ Data encoding strategy
  -> Dynamic t (f a) -- ^ Collection of data
  -> m (Event t e)
dynamicCapped tableAttrs headAttrs bodyAttrs trAttrs fascia cornice collection =
  elDynAttr "table" tableAttrs $ do
    -- TODO: Figure out what this ignored argument represents and dont ignore it
    _ <- encodeCorniceHeadDynamic headAttrs fascia (E.annotate cornice)
    dynamicBody bodyAttrs trAttrs (E.discard cornice) collection

-- | Table with cells that can create expanded content
--   between the rows.
expandable :: (MonadWidget t m, Foldable f)
  => Dynamic t (M.Map T.Text T.Text) -- ^ @\<table\>@ tag attributes
  -> Dynamic t (M.Map T.Text T.Text) -- ^ Attributes of expanded @\<td\>@
  -> f a -- ^ Values
  -> Colonnade Headed a (Cell t m (Event t (Maybe (m ()))))
     -- ^ Encoding into cells with events that can fire to create additional content under the row
  -> m ()
expandable tableAttrs tdExpandedAttrs as encoding@(E.Colonnade v) = do
  let vlen = V.length v
  elDynAttr "table" tableAttrs $ do
    -- Discarding this result is technically the wrong thing
    -- to do, but I cannot imagine why anyone would want to
    -- drop down content under the heading.
    _ <- el "thead" $ el "tr" $ E.headerMonadicGeneral_ encoding (elFromCell "th")
    el "tbody" $ forM_ as $ \a -> do
      e' <- el "tr" $ do
        elist <- E.rowMonadicWith [] (++) encoding (fmap (\k -> [k]) . elFromCell "td") a
        let e = leftmost elist
            e' = flip fmap e $ \mwidg -> case mwidg of
              Nothing -> return ()
              Just widg -> el "tr" $ do
                elDynAttr "td" (M.insert "colspan" (T.pack (show vlen)) <$> tdExpandedAttrs) widg
        return e'
      widgetHold (return ()) e'

