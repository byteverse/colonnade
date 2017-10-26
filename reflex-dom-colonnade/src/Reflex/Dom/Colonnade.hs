{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Reflex.Dom.Colonnade
  (
  -- * Types
    Cell(..)
  , Resizable(..)
  , Bureau(..)
  , Arrangement(..)
  , Pagination(..)
  -- * Typeclasses
  , Cellular(..)
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
  -- , expandableResizableTableless
  , sectioned
  , paginated
  , paginatedExpandable
    -- * Cell Functions
  , cell
  , charCell
  , stringCell
  , textCell
  , lazyTextCell
  , builderCell
  , headedResizable
    -- * Other Stuff
  , defBureau
    -- * Pagination
  , semUiFixedPagination
  ) where

import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Profunctor as PF
import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Data.Text (Text)
import Data.Foldable (Foldable(..),for_,forM_,foldlM)
import Data.Traversable (for)
import Data.Semigroup (Semigroup(..))
import Control.Applicative (liftA2)
import Reflex.Dom
import Colonnade (Colonnade,Headed,Headless,Fascia,Cornice,Headedness(..))
import Data.Monoid (Sum(..))
import Data.Proxy
import Control.Monad.Fix (MonadFix)
import Control.Monad (forM)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Colonnade as C
import qualified Colonnade.Encode as E

data Cell t m b = Cell
  { cellAttrs :: !(Dynamic t (M.Map T.Text T.Text))
  , cellContents :: !(m b)
  } deriving (Functor)

-- | In practice, this size will only ever be set to zero
--   or one.
data Resizable t h b = Resizable
  { resizableSize :: !(Dynamic t Int)
  , resizableContent :: !(h b)
  } deriving (Foldable, Functor)

data Bureau t h a = Bureau
  { bureauTable :: Dynamic t (Map Text Text)
    -- ^ attributes of @\<table\>@
  , bureauHead :: h (Dynamic t (Map Text Text), Dynamic t (Map Text Text))
    -- ^ attributes of @\<thead\>@ and of the @\<tr\>@ inside of it.
  , bureauBody :: Dynamic t (Map Text Text)
  , bureauRow :: (a -> Dynamic t (Map Text Text))
    -- ^ attributes of each @\<tr\>@, based on the element
  }
  -- , bureauHeadRow :: h (Dynamic t (Map Text Text))

data Pagination t m = Pagination
  { paginationRows :: Int
    -- ^ Maximum number of rows on a page
  , paginationArrangement :: Arrangement t
    -- ^ Where pagination is situated relative to table
  , paginationContent :: Dynamic t Int -> m (Dynamic t Int)
    -- ^ The argument to this function is an @Dynamic@ that carries
    --   the total number of pages that should be available. When
    --   this dynamic changes, it means that the rows backing the
    --   table have been changed. Typically, this should cause
    --   the @Dynamic@ in the return value to reset to 0. This
    --   returned @Dynamic@ represents the current page.
  }

-- | Where the pagination goes relative to the table
data Arrangement t
  = ArrangementAbove
  | ArrangementBeneath
  | ArrangementFooter
      (Dynamic t (Map Text Text))
      (Dynamic t (Map Text Text))
      (Dynamic t (Map Text Text))
      -- ^ contains attributes of @\<tfoot\>@, its inner @\<tr\>@, and its inner @\<th\>@.

-- | Things that can be rendered as cells in a table.
class (PostBuild t m, DomBuilder t m) => Cellular t m c | c -> m, c -> t where
  cellularAttrs :: c b -> Dynamic t (Map Text Text)
  cellularContents :: c b -> m b

instance (PostBuild t m, DomBuilder t m) => Cellular t m (Cell t m) where
  cellularAttrs = cellAttrs
  cellularContents = cellContents

instance (Reflex t, DomBuilder t m, PerformEvent t m, MonadHold t m, MonadFix m) => Cellular t (PostBuildT t m) (PostBuildT t m) where
  cellularAttrs _ = pure M.empty
  cellularContents = id

instance Cellular t m m => Cellular t (ReaderT r m) (ReaderT r m) where
  cellularAttrs _ = pure M.empty
  cellularContents = id
  
instance (Cellular t m m, MonadHold t m, MonadFix m, Semigroup w) => Cellular t (EventWriterT t w m) (EventWriterT t w m) where
  cellularAttrs _ = pure M.empty
  cellularContents = id
  

-- | This typeclass is provided to make using functions in this
--   library more convenient. The methods could have been passed
--   around in a dictionary instead, but there is only really one
--   sensible implementation for each header type. The only
--   law it should satisfy is:
--
--   > sizableSize (headednessPure Proxy x) == pure 1
--
--   Also, since the instances we are interested in preclude
--   the use of a functional dependency, the typeclass is annoying
--   to use. But, end users should never need to use it.
class Sizable t b h | h -> b where
  sizableSize :: h a -> Dynamic t Int
  sizableCast :: Proxy t -> h a -> b a

-- instance (Headedness h, Reflex t) => Headedness (Resizable t h) where
--   headednessPure = Resizable (pure 1) . headednessPure
--   headednessContents = do
--     f <- headednessContents
--     Just (\(Resizable _ a) -> f a)

instance (Headedness h, Reflex t) => Sizable t h (Resizable t h) where
  sizableSize = resizableSize
  sizableCast _ (Resizable _ h) = h

instance Reflex t => Sizable t Headed Headed where
  sizableSize _ = pure 1
  sizableCast _ = id

instance Reflex t => Sizable t Headless Headless where
  sizableSize _ = pure 1
  sizableCast _ = id

defBureau :: forall t h a. (Reflex t, Headedness h) => Bureau t h a
defBureau = Bureau
  { bureauTable = pure M.empty
  , bureauHead = headednessPure (pure M.empty, pure M.empty)
  , bureauBody = pure M.empty
  , bureauRow = const (pure M.empty)
  }

elFromCell :: (DomBuilder t m, PostBuild t m) => T.Text -> Cell t m b -> m b
elFromCell e (Cell attr m) = elDynAttr e attr m

-- elFromCellular :: (Cellular t m c, PostBuild t m, DomBuilder t m)
--   => T.Text -- name of the element, @th@ or @td@
--   -> c b -- cellular value
--   -> m b
-- elFromCellular name c = elDynAttr name (cellularAttrs c) (cellularContents c)

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

headedResizable :: Dynamic t Int -> c -> (a -> c) -> Colonnade (Resizable t Headed) a c
headedResizable d c = C.singleton (Resizable d (E.Headed c))

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
  => Dynamic t (M.Map T.Text T.Text)
  -> (a -> Dynamic t (M.Map T.Text T.Text))
  -> Colonnade h a (Cell t m e)
  -> f a
  -> m e
body bodyAttrs trAttrs colonnade collection =
  elDynAttr "tbody" bodyAttrs (bodyRows trAttrs colonnade collection)

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
  (DomBuilder t m, PostBuild t m, Foldable f, Headedness h, Monoid e)
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
    body (pure bodyAttrs) (pure . trAttrs) colonnade collection

staticTableless ::
  (DomBuilder t m, PostBuild t m, Foldable f, Headedness h, Monoid e)
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
  body (pure bodyAttrs) trAttrs colonnade collection

-- | A table dividing into sections by @\<td\>@ elements that
--   take up entire rows.
sectioned :: 
  (DomBuilder t m, PostBuild t m, Foldable f, Headedness h, Foldable g)
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
    b <- body (pure bodyAttrs) (pure . trAttrs) (E.discard cornice) collection
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
  (DomBuilder t m, PostBuild t m, Foldable f, Headedness h, Semigroup e, Monoid e)
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

-- expandableResizableTableless :: forall t m f a b. (MonadWidget t m, Foldable f)
--   => f a -- ^ Values
--   -> (Event t b -> m ())
--      -- ^ Encoding over additional content
--   -> Colonnade (Resizable t Headed) a (m (Event t (Maybe b)))
--      -- ^ Encoding into cells with events that can fire to create additional content under the row
--   -> m ()
-- expandableResizableTableless as expansion encoding@(E.Colonnade v) = do
--   let vlen = coerceDynamic (foldMap (\(E.OneColonnade (Resizable sz _) _) -> coerceDynamic sz :: Dynamic t (Sum Int)) v) :: Dynamic t (Sum Int)
--       totalSizeAttr = fmap (\i -> M.singleton "colspan" (T.pack (show i))) vlen
--   _ <- el "thead" $ el "tr" $ E.headerMonadicGeneral_ encoding (el "th")
--   el "tbody" $ forM_ as $ \a -> do
--     x <- el "tr" $ E.rowMonadicWith [] (++) encoding (fmap (\k -> [k]) . el "td") a
--     let e = leftmost x
--     d <- holdDyn Nothing e
--     elDynAttr "tr" (fmap (maybe (M.singleton "style" "display:none;") (const M.empty)) d) $ do
--       elDynAttr "td" totalSizeAttr (expansion (fmapMaybe id e))

data Visible a = Visible !Bool a

-- TODO: figure out a way to get rid of the awful default value hack
-- It would be nice to use foldDynMaybeM, but we still need an initial
-- value. We could try to wait to generate the rows until we've seen
-- a value, but that seems confusing.
paginated :: forall t b h m a c e.
     (Sizable t b h, Cellular t m c, Headedness b, MonadFix m, Functor h, Monoid e)
  => Bureau t b a -- ^ table class settings
  -> Pagination t m -- ^ pagination settings
  -> a -- ^ An inhabitant of type @a@ only used for the cells in hidden rows.
  -> Colonnade h (Dynamic t a) (c e) -- ^ column blueprint
  -> Dynamic t (Vector a) -- ^ table row data
  -> m e
paginated (Bureau tableAttrs theadAttrs bodyAttrs trAttrs) (Pagination pageSize arrange makePagination) aDef col vecD = do
  let colLifted :: Colonnade h (Dynamic t (Visible a)) (c e)
      colLifted = PF.lmap (fmap (\(Visible _ a) -> a)) col
      makeVals :: Dynamic t Int -> Vector (Dynamic t (Visible a))
      makeVals page = V.generate pageSize $ \ix -> do
        p <- page
        v <- vecD
        return (maybe (Visible False aDef) (Visible True) (v V.!? (p * pageSize + ix)))
      totalPages :: Dynamic t Int
      totalPages = fmap ((`divRoundUp` pageSize) . V.length) vecD
      hideWhenUnipage :: Dynamic t (Map Text Text) -> Dynamic t (Map Text Text)
      hideWhenUnipage = zipDynWith
        ( \ct attrs -> if ct > 1 then attrs else M.insert "style" "display:none;" attrs
        ) totalPages
      trAttrsLifted :: Dynamic t (Visible a) -> Dynamic t (Map Text Text)
      trAttrsLifted d = do
        Visible isVisible a <- d
        attrs <- trAttrs a
        return (if isVisible then attrs else M.insertWith T.append "style" "display:none;" attrs)
      size :: Dynamic t Int
      size = coerceDynamic (foldMap (\x -> coerceDynamic (sizableSize (E.oneColonnadeHead x)) :: Dynamic t (Sum Int)) (E.getColonnade col))
  elDynAttr "table" tableAttrs $ case arrange of
    ArrangementFooter tfootAttrs tfootTrAttrs tfootThAttrs -> mdo
      tableHeader theadAttrs colLifted
      let vals = makeVals page
      e <- tableBody bodyAttrs trAttrsLifted colLifted vals
      page <- elDynAttr "tfoot" (hideWhenUnipage tfootAttrs) $ do
        elDynAttr "tr" tfootTrAttrs $ do
          let attrs = zipDynWith insertSizeAttr size tfootThAttrs
          elDynAttr "th" attrs $ do
            makePagination totalPages
      return e
    _ -> error "Reflex.Dom.Colonnade: paginated: write this code"

-- | A paginated table with a fixed number of rows. Each row can
--   expand a section beneath it, represented as an additional
--   table row. CSS rules that give the table a striped appearance
--   are unlikely to work since there are hidden rows.
paginatedExpandable :: forall t b h m a c.
     (Sizable t b h, Cellular t m c, Headedness b, MonadFix m, Functor h, MonadHold t m)
  => Bureau t b a -- ^ table class settings
  -> Pagination t m -- ^ pagination settings
  -> a -- ^ An inhabitant of type @a@ only used for the cells in hidden rows.
  -> (Dynamic t a -> m ()) -- expandable extra content
  -> Colonnade h (Dynamic t a) (c (Dynamic t Bool))
     -- ^ Column blueprint. The boolean event enables and disables the expansion.
  -> Dynamic t (Vector a) -- ^ table row data
  -> m ()
paginatedExpandable (Bureau tableAttrs theadAttrs bodyAttrs trAttrs) (Pagination pageSize arrange makePagination) aDef expansion col vecD = do
  let colLifted :: Colonnade h (Dynamic t (Visible a)) (c (Dynamic t Bool))
      colLifted = PF.lmap (fmap (\(Visible _ a) -> a)) col
      expansionLifted :: Dynamic t (Visible a) -> m ()
      expansionLifted = expansion . fmap (\(Visible _ a) -> a)
      makeVals :: Dynamic t Int -> Vector (Dynamic t (Visible a))
      makeVals page = V.generate pageSize $ \ix -> do
          p <- page
          v <- vecD
          return (maybe (Visible False aDef) (Visible True) (v V.!? (p * pageSize + ix)))
      totalPages :: Dynamic t Int
      totalPages = fmap ((`divRoundUp` pageSize) . V.length) vecD
      hideWhenUnipage :: Dynamic t (Map Text Text) -> Dynamic t (Map Text Text)
      hideWhenUnipage = zipDynWith
        ( \ct attrs -> if ct > 1 then attrs else M.insert "style" "display:none;" attrs
        ) totalPages
      -- trAttrsLifted :: Visible a -> Dynamic t (Map Text Text)
      -- trAttrsLifted (Visible _ a) = trAttrs a
      trAttrsLifted :: Dynamic t (Visible a) -> Dynamic t (Map Text Text)
      trAttrsLifted d = do
        Visible isVisible a <- d
        attrs <- trAttrs a
        return (if isVisible then attrs else M.insertWith T.append "style" "display:none;" attrs)
      size :: Dynamic t Int
      size = coerceDynamic (foldMap (\x -> coerceDynamic (sizableSize (E.oneColonnadeHead x)) :: Dynamic t (Sum Int)) (E.getColonnade col))
  elDynAttr "table" tableAttrs $ case arrange of
    ArrangementFooter tfootAttrs tfootTrAttrs tfootThAttrs -> mdo
      tableHeader theadAttrs colLifted
      let vals = makeVals page
      tableBodyExpandable size expansionLifted bodyAttrs trAttrsLifted colLifted vals (Visible True aDef)
      page <- elDynAttr "tfoot" (hideWhenUnipage tfootAttrs) $ do
        elDynAttr "tr" tfootTrAttrs $ do
          let attrs = zipDynWith insertSizeAttr size tfootThAttrs
          elDynAttr "th" attrs $ do
            makePagination totalPages
      return ()
    _ -> error "Reflex.Dom.Colonnade: paginated: write this code"

 
divRoundUp :: Int -> Int -> Int
divRoundUp a b = case divMod a b of
  (x,y) -> if y == 0 then x else x + 1
    
tableHeader :: forall t b h c a m x.
     (Reflex t, Sizable t b h, Cellular t m c, Headedness b)
  => b (Dynamic t (Map Text Text), Dynamic t (Map Text Text))
  -> Colonnade h a (c x)
  -> m ()
tableHeader theadAttrsWrap col = case headednessExtractForall of
  Nothing -> return ()
  Just extractForall -> do
    let (theadAttrs,trAttrs) = extract theadAttrsWrap
    elDynAttr "thead" theadAttrs $ do
      elDynAttr "tr" trAttrs $ do
        headerMonadicGeneralSizable_ col (extract . sizableCast (Proxy :: Proxy t))
    where
    extract :: forall y. b y -> y
    extract = E.runExtractForall extractForall
    
tableBody :: (DomBuilder t m, PostBuild t m, Foldable f, Monoid e, Cellular t m c, Sizable t b h)
  => Dynamic t (M.Map T.Text T.Text)
  -> (a -> Dynamic t (M.Map T.Text T.Text))
  -> Colonnade h a (c e)
  -> f a
  -> m e
tableBody bodyAttrs trAttrs col collection =
  elDynAttr "tbody" bodyAttrs $ foldlM (\m a -> do
      e <- elDynAttr "tr" (trAttrs a) (rowSizable col a)
      return (mappend m e)
    ) mempty collection

-- | This function has a implementation that is careful to only
--   redraw the expansion rows, which are usually hidden, when
--   it is necessary to do so.
tableBodyExpandable :: forall t m c b a h. (DomBuilder t m, MonadHold t m, PostBuild t m, Cellular t m c, Sizable t b h)
  => Dynamic t Int -- ^ number of visible columns in the table
  -> (Dynamic t a -> m ())
  -> Dynamic t (M.Map T.Text T.Text)
  -> (Dynamic t a -> Dynamic t (M.Map T.Text T.Text))
  -> Colonnade h (Dynamic t a) (c (Dynamic t Bool))
  -> Vector (Dynamic t a)
  -> a -- ^ initial value, a hack
  -> m ()
tableBodyExpandable colCount renderExpansion bodyAttrs trAttrs col collection a0 =
  elDynAttr "tbody" bodyAttrs $ mapM_ (\a -> do
      let attrs = trAttrs a
      expanded <- elDynAttr "tr" attrs (rowSizableReified (return False) (zipDynWith (||)) col a)
      visibleVal <- gateDynamic expanded a0 a
      elDynAttr "tr" (zipDynWith insertVisibilityAttr expanded attrs) $ do
        -- TODO: possibly provide a way to customize these attributes
        let expansionTdAttrs = pure M.empty
        elDynAttr "td" (zipDynWith insertSizeAttr colCount expansionTdAttrs) (renderExpansion visibleVal)
    ) collection

-- | Create a dynamic whose value only updates when the gate is 'True'.
--   This dynamic starts out with the original value of its input
--   regardless of whether the gate is true or false.
gateDynamic :: (MonadHold t m, Reflex t) => Dynamic t Bool -> a -> Dynamic t a -> m (Dynamic t a)
gateDynamic g a0 a = do
  -- TODO: throw a nubDynWith in here
  let e = fmapMaybe id (updated (zipDynWith (\b v -> if b then Just v else Nothing) g a))
  holdDyn a0 e

headerMonadicGeneralSizable_ :: (Sizable t b h, Cellular t m c)
  => Colonnade h a (c x)
  -> (h (c x) -> c x)
  -> m ()
headerMonadicGeneralSizable_ (E.Colonnade v) extract = 
  V.mapM_ go v
  where
  go x = do
    let h = E.oneColonnadeHead x
        c = extract h
        attrs = zipDynWith insertSizeAttr (sizableSize h) (cellularAttrs c)
    elDynAttr "th" attrs (cellularContents c)

rowSizableReified :: (Sizable t b h, Cellular t m c)
  => e -- ^ identity element
  -> (e -> e -> e) -- ^ associative append
  -> Colonnade h a (c e)
  -> a
  -> m e
rowSizableReified theEmpty theAppend (E.Colonnade v) a = V.foldM (\m oc -> do
    let c = E.oneColonnadeEncode oc a
        sz = sizableSize (E.oneColonnadeHead oc)
        attrs = zipDynWith insertSizeAttr sz (cellularAttrs c)
    e <- elDynAttr "td" attrs $ do
      cellularContents c
    return (theAppend m e)
  ) theEmpty v

rowSizable :: (Sizable t b h, Cellular t m c, Monoid e)
  => Colonnade h a (c e)
  -> a
  -> m e
rowSizable (E.Colonnade v) a = V.foldM (\m oc -> do
    let c = E.oneColonnadeEncode oc a
        sz = sizableSize (E.oneColonnadeHead oc)
        attrs = zipDynWith insertSizeAttr sz (cellularAttrs c)
    e <- elDynAttr "td" attrs $ do
      cellularContents c
    return (mappend m e)
  ) mempty v

insertVisibilityAttr :: Bool -> Map Text Text -> Map Text Text
insertVisibilityAttr b m = case b of
  False -> M.insertWith T.append "style" "display:none;" m
  True -> m

insertSizeAttr :: Int -> Map Text Text -> Map Text Text
insertSizeAttr i m
  | i < 1 = M.insertWith T.append "style" "display:none;" m
  | otherwise = M.insert "colspan" (T.pack (show i)) m

-- | only used internally for implementations of 'Pagination'.
data Movement = Forward | Backward | Position {-# UNPACK #-} !Int

-- | Pagination using the classes and DOM layout that Semantic UI
--   expects. The function will typically be partially applided
--   to the first two arguments to make it suitable as a field
--   of 'Pagination'.
semUiFixedPagination :: MonadWidget t m
  => Int -- ^ Maximum allowed number of pages.
  -> Text -- ^ Extra classes to be applied. Already included is @ui pagination menu@.
  -> Dynamic t Int
  -> m (Dynamic t Int)
semUiFixedPagination maxPageCount extraClass pageCount = do
  elClass "div" (T.append "ui pagination menu " extraClass) $ mdo
    (bckEl,()) <- elClass' "a" "icon item" $ do
      elClass "i" "left chevron icon" (return ())
    let bck = Backward <$ domEvent Click bckEl
    posList <- forM (enumFromTo 0 (maxPageCount - 1)) $ \i -> do
      let attrs = zipDynWith (\ct pg -> M.unionsWith (<>)
              [ if i < ct then M.empty else M.singleton "style" "display:none;"
              , if i == pg then M.singleton "class" " active " else M.empty
              , M.singleton "class" " item "
              ]
            ) pageCount page
      (pageEl, ()) <- elDynAttr' "a" attrs (text (T.pack (show i)))
      return (Position i <$ domEvent Click pageEl)
    (fwdEl,()) <- elClass' "a" "icon item" $ do
      elClass "i" "right chevron icon" (return ())
    let fwd = Forward <$ domEvent Click fwdEl
    let moveEv = leftmost (fwd : bck : (Position 0 <$ updated pageCount) : posList)
    page <- foldDynM (\move oldPage -> case move of
        Backward -> return (max 0 (oldPage - 1))
        Forward -> do
          nowPageCount <- sample (current pageCount)
          return (min (nowPageCount - 1) (oldPage + 1))
        Position updatedPage -> return updatedPage
      ) 0 moveEv
    holdUniqDyn page
