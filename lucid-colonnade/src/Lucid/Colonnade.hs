{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Build HTML tables using @lucid@ and @colonnade@. It is
--   recommended that users read the documentation for @colonnade@ first,
--   since this library builds on the abstractions introduced there.
--   Also, look at the docs for @blaze-colonnade@. These two
--   libraries are similar, but blaze offers an HTML pretty printer
--   which makes it possible to doctest examples. Since lucid
--   does not offer such facilities, examples are omitted here.
module Lucid.Colonnade
  ( -- * Apply
    encodeHtmlTable
  , encodeCellTable
  , encodeCellTableSized
  , encodeTable
    -- * Cell
    -- $build
  , Cell(..)
  , htmlCell
  , stringCell
  , textCell
  , lazyTextCell
  , builderCell
  , htmlFromCell
  , encodeBodySized
    -- * Discussion
    -- $discussion
  ) where

import Colonnade (Colonnade,Headed,Headless,Fascia,Cornice)
import Data.Text (Text)
import Control.Monad
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Data.Foldable
import Data.String (IsString(..))
import Data.Maybe (listToMaybe)
import Data.Char (isSpace)
import Control.Applicative (liftA2)
import Lucid hiding (for_)
import qualified Colonnade as Col
import qualified Data.List as List
import qualified Colonnade.Encode as E
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.Vector as V
import qualified Data.Text as T

-- $build
--
-- The 'Cell' type is used to build a 'Colonnade' that 
-- has 'Html' content inside table cells and may optionally
-- have attributes added to the @\<td\>@ or @\<th\>@ elements
-- that wrap this HTML content.

-- | The attributes that will be applied to a @\<td\>@ and
--   the HTML content that will go inside it. When using
--   this type, remember that 'Attribute', defined in @blaze-markup@,
--   is actually a collection of attributes, not a single attribute.
data Cell d = Cell
  { cellAttribute :: ![Attribute]
  , cellHtml :: !(Html d)
  }

instance (d ~ ()) => IsString (Cell d) where
  fromString = stringCell

instance Semigroup d => Semigroup (Cell d) where
  Cell a1 c1 <> Cell a2 c2 = Cell (mappend a1 a2) (liftA2 (<>) c1 c2)

instance Monoid d => Monoid (Cell d) where
  mempty = Cell mempty (return mempty)
  mappend (Cell a1 c1) (Cell a2 c2) = Cell (mappend a1 a2) (liftA2 mappend c1 c2)

-- | Create a 'Cell' from a 'Widget'
htmlCell :: Html d -> Cell d
htmlCell = Cell mempty

-- | Create a 'Cell' from a 'String'
stringCell :: String -> Cell ()
stringCell = htmlCell . fromString

-- | Create a 'Cell' from a 'Char'
charCell :: Char -> Cell ()
charCell = stringCell . pure

-- | Create a 'Cell' from a 'Text'
textCell :: Text -> Cell ()
textCell = htmlCell . toHtml

-- | Create a 'Cell' from a lazy text
lazyTextCell :: LText.Text -> Cell ()
lazyTextCell = textCell . LText.toStrict

-- | Create a 'Cell' from a text builder
builderCell :: TBuilder.Builder -> Cell ()
builderCell = lazyTextCell . TBuilder.toLazyText

-- | Encode a table. Table cell element do not have 
--   any attributes applied to them.
encodeHtmlTable :: 
     (E.Headedness h, Foldable f, Monoid d)
  => [Attribute] -- ^ Attributes of @\<table\>@ element
  -> Colonnade h a (Html d) -- ^ How to encode data as columns
  -> f a -- ^ Collection of data
  -> Html d
encodeHtmlTable = encodeTable
  (E.headednessPure ([],[])) mempty (const mempty) (\el -> el [])

-- | Encode a table. Table cells may have attributes applied
--   to them
encodeCellTable :: 
     (E.Headedness h, Foldable f, Monoid d)
  => [Attribute] -- ^ Attributes of @\<table\>@ element
  -> Colonnade h a (Cell d) -- ^ How to encode data as columns
  -> f a -- ^ Collection of data
  -> Html d
encodeCellTable = encodeTable
  (E.headednessPure ([],[])) mempty (const mempty) htmlFromCell

encodeCellTableSized :: 
     (E.Headedness h, Foldable f, Monoid d)
  => [Attribute] -- ^ Attributes of @\<table\>@ element
  -> Colonnade (E.Sized Int h) a (Cell d) -- ^ How to encode data as columns
  -> f a -- ^ Collection of data
  -> Html ()
encodeCellTableSized = encodeTableSized
  (E.headednessPure ([],[])) mempty (const mempty) htmlFromCell

-- | Encode a table. This handles a very general case and
--   is seldom needed by users. One of the arguments provided is
--   used to add attributes to the generated @\<tr\>@ elements.
--   The elements of type @d@ produced by generating html are
--   strictly combined with their monoidal append function.
--   However, this type is nearly always @()@.
encodeTable :: forall f h a d c.
     (Foldable f, E.Headedness h, Monoid d)
  => h ([Attribute],[Attribute]) -- ^ Attributes of @\<thead\>@ and its @\<tr\>@
  -> [Attribute] -- ^ Attributes of @\<tbody\>@ element
  -> (a -> [Attribute]) -- ^ Attributes of each @\<tr\>@ element
  -> (([Attribute] -> Html d -> Html d) -> c -> Html d) -- ^ Wrap content and convert to 'Html'
  -> [Attribute] -- ^ Attributes of @\<table\>@ element
  -> Colonnade h a c -- ^ How to encode data as a row
  -> f a -- ^ Collection of data
  -> Html d
encodeTable mtheadAttrs tbodyAttrs trAttrs wrapContent tableAttrs colonnade xs =
  table_ tableAttrs $ do
    d1 <- case E.headednessExtractForall of
      Nothing -> return mempty
      Just extractForall -> do
        let (theadAttrs,theadTrAttrs) = extract mtheadAttrs
        thead_ theadAttrs $ tr_ theadTrAttrs $ do
          foldlMapM' (wrapContent th_ . extract . E.oneColonnadeHead) (E.getColonnade colonnade)
        where
        extract :: forall y. h y -> y
        extract = E.runExtractForall extractForall
    d2 <- encodeBody trAttrs wrapContent tbodyAttrs colonnade xs
    return (mappend d1 d2)

encodeBody :: (Foldable f, Monoid d)
  => (a -> [Attribute]) -- ^ Attributes of each @\<tr\>@ element
  -> (([Attribute] -> Html d -> Html d) -> c -> Html d) -- ^ Wrap content and convert to 'Html'
  -> [Attribute] -- ^ Attributes of @\<tbody\>@ element
  -> Colonnade h a c -- ^ How to encode data as a row
  -> f a -- ^ Collection of data
  -> Html d
encodeBody trAttrs wrapContent tbodyAttrs colonnade xs = do
  tbody_ tbodyAttrs $ do
    flip foldlMapM' xs $ \x -> do
      tr_ (trAttrs x) $ E.rowMonadic colonnade (wrapContent td_) x

encodeBodySized ::
     (Foldable f, Monoid d)
  => (a -> [Attribute]) 
  -> [Attribute]
  -> Colonnade (E.Sized Int h) a (Cell d) 
  -> f a
  -> Html ()
encodeBodySized trAttrs tbodyAttrs colonnade collection = tbody_ tbodyAttrs $ do
  flip foldMap collection $ \a -> tr_ (trAttrs a) $ do
    E.rowMonoidalHeader 
      colonnade 
      (\(E.Sized sz _) (Cell cattr content) -> 
        void $ td_ (setColspanOrHide sz cattr) content
      )
      a

encodeTableSized :: forall f h a d c.
     (Foldable f, E.Headedness h, Monoid d)
  => h ([Attribute],[Attribute]) -- ^ Attributes of @\<thead\>@ and its @\<tr\>@
  -> [Attribute] -- ^ Attributes of @\<tbody\>@ element
  -> (a -> [Attribute]) -- ^ Attributes of each @\<tr\>@ element
  -> (([Attribute] -> Html d -> Html d) -> (Cell d) -> Html d) -- ^ Wrap content and convert to 'Html'
  -> [Attribute] -- ^ Attributes of @\<table\>@ element
  -> Colonnade (E.Sized Int h) a (Cell d) -- ^ How to encode data as a row
  -> f a -- ^ Collection of data
  -> Html ()
encodeTableSized mtheadAttrs tbodyAttrs trAttrs wrapContent tableAttrs colonnade xs =
  table_ tableAttrs $ do
    d1 <- case E.headednessExtractForall of
      Nothing -> pure mempty
      Just extractForall -> do
        let (theadAttrs,theadTrAttrs) = extract mtheadAttrs
        thead_ theadAttrs $ tr_ theadTrAttrs $ do
          foldlMapM' 
            (wrapContent th_ . extract . 
              (\(E.Sized i h) -> case E.headednessExtract of 
                  Just f -> 
                      let (Cell attrs content) = f h 
                       in E.headednessPure $ Cell (setColspanOrHide i attrs) content
                  Nothing -> E.headednessPure mempty
                  -- (E.Headed (Cell attrs content)) -> E.Headed $ Cell (setColspanOrHide i attrs) content
                  -- E.Headless -> E.Headless
              )
              . E.oneColonnadeHead
            )
            (E.getColonnade colonnade)
        where
        extract :: forall y. h y -> y
        extract = E.runExtractForall extractForall
    encodeBodySized trAttrs tbodyAttrs colonnade xs
    pure ()

setColspanOrHide :: Int -> [Attribute] -> [Attribute]
setColspanOrHide i attrs
  | i < 1 = style_ "display:none;" : attrs
  | otherwise = colspan_ (Text.pack (show i)) : attrs

foldlMapM' :: forall g b a m. (Foldable g, Monoid b, Monad m) => (a -> m b) -> g a -> m b
foldlMapM' f xs = foldr f' pure xs mempty
  where
  f' :: a -> (b -> m b) -> b -> m b
  f' x k bl = do
    br <- f x
    let !b = mappend bl br
    k b

-- | Convert a 'Cell' to 'Html' by wrapping the content with a tag
-- and applying the 'Cell' attributes to that tag.
htmlFromCell :: ([Attribute] -> Html d -> Html d) -> Cell d -> Html d
htmlFromCell f (Cell attr content) = f attr content

-- $discussion
--
-- In this module, some of the functions for applying a 'Colonnade' to
-- some values to build a table have roughly this type signature:
--
-- > Foldable a => Colonnade Headedness a (Cell d) -> f a -> Html d
--
-- The 'Colonnade' content type is 'Cell', but the content
-- type of the result is 'Html'. It may not be immidiately clear why
-- this is done. Another strategy, which this library also
-- uses, is to write
-- these functions to take a 'Colonnade' whose content is 'Html':
--
-- > Foldable a => Colonnade Headedness a (Html d) -> f a -> Html d
--
-- When the 'Colonnade' content type is 'Html', then the header
-- content is rendered as the child of a @\<th\>@ and the row
-- content the child of a @\<td\>@. However, it is not possible
-- to add attributes to these parent elements. To accomodate this
-- situation, it is necessary to introduce 'Cell', which includes
-- the possibility of attributes on the parent node.

sectioned :: 
  (Foldable f, E.Headedness h, Foldable g, Monoid c)
  => [Attribute] -- ^ @\<table\>@ tag attributes
  -> Maybe ([Attribute], [Attribute])
  -- ^ Attributes of @\<thead\>@ and its @\<tr\>@, pass 'Nothing' to omit @\<thead\>@
  -> [Attribute] -- ^ @\<tbody\>@ tag attributes
  -> (a -> [Attribute]) -- ^ @\<tr\>@ tag attributes for data rows
  -> (b -> Cell c) -- ^ Section divider encoding strategy
  -> Colonnade h a (Cell c) -- ^ Data encoding strategy
  -> f (b, g a) -- ^ Collection of data
  -> Html ()
sectioned tableAttrs mheadAttrs bodyAttrs trAttrs dividerContent colonnade@(E.Colonnade v) collection = do
  pure ()
  let vlen = V.length v
  table_ tableAttrs $ do
    for_ mheadAttrs $ \(headAttrs,headTrAttrs) ->
      thead_ headAttrs . tr_ headTrAttrs $
        E.headerMonadicGeneral_ colonnade (htmlFromCell th_)
    tbody_ bodyAttrs $ forM_ collection $ \(b,as) -> do
      let Cell attrs contents = dividerContent b
      tr_ [] $ do
        td_ ((colspan_ $ T.pack (show vlen)): attrs) contents
      flip foldlMapM' as $ \a -> do
        tr_ (trAttrs a) $ E.rowMonadic colonnade (htmlFromCell td_) a

