{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK not-home #-}

{- | Most users of this library do not need this module. The functions
  here are used to build functions that apply a 'Colonnade'
  to a collection of values, building a table from them. Ultimately,
  a function that applies a @Colonnade Headed MyCell a@
  to data will have roughly the following type:

> myTableRenderer :: Foldable g => Colonnade Headed MyCell a -> g a -> MyContent

  In the companion packages @yesod-colonnade@ and
  @reflex-dom-colonnade@, functions with
  similar type signatures are readily available.
  These packages use the functions provided here
  in the implementations of their rendering functions.
  It is recommended that users who believe they may need
  this module look at the source of the companion packages
  to see an example of how this module\'s functions are used.
  Other backends are encouraged to use these functions
  to build monadic or monoidal content from a 'Colonnade'.

  The functions exported here take a 'Colonnade' and
  convert it to a fragment of content. The functions whose
  names start with @row@ take at least a @Colonnade f c a@ and an @a@
  value to generate a row of content. The functions whose names
  start with @header@ need the @Colonnade f c a@ but not
  an @a@ value since a value is not needed to build a header.
-}
module Colonnade.Encode
  ( -- * Colonnade

    -- ** Types
    Colonnade (..)
  , OneColonnade (..)
  , Headed (..)
  , Headless (..)
  , Sized (..)
  , ExtractForall (..)

    -- ** Typeclasses
  , Headedness (..)

    -- ** Row
  , row
  , rowMonadic
  , rowMonadic_
  , rowMonadicWith
  , rowMonoidal
  , rowMonoidalHeader

    -- ** Header
  , header
  , headerMonadic
  , headerMonadic_
  , headerMonadicGeneral
  , headerMonadicGeneral_
  , headerMonoidalGeneral
  , headerMonoidalFull

    -- ** Other
  , bothMonadic_
  , sizeColumns

    -- * Cornice

    -- ** Types
  , Cornice (..)
  , AnnotatedCornice (..)
  , OneCornice (..)
  , Pillar (..)
  , ToEmptyCornice (..)
  , Fascia (..)

    -- ** Encoding
  , annotate
  , annotateFinely
  , size
  , endow
  , discard
  , headersMonoidal
  , uncapAnnotated
  ) where

import Control.Monad.ST (ST, runST)
import Data.Foldable
import Data.Functor.Contravariant (Contravariant (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Profunctor (Profunctor (..))
import Data.Vector (Vector)

import qualified Data.Semigroup as Semigroup
import qualified Data.Vector as V
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU

{- | Consider providing a variant the produces a list
instead. It may allow more things to get inlined
in to a loop.
-}
row :: (c1 -> c2) -> Colonnade f a c1 -> a -> Vector c2
row g (Colonnade v) a = flip Vector.map v $
  \(OneColonnade _ encode) -> g (encode a)

bothMonadic_ ::
  (Monad m) =>
  Colonnade Headed a c ->
  (c -> c -> m b) ->
  a ->
  m ()
bothMonadic_ (Colonnade v) g a =
  forM_ v $ \(OneColonnade (Headed h) encode) -> g h (encode a)

rowMonadic ::
  (Monad m, Monoid b) =>
  Colonnade f a c ->
  (c -> m b) ->
  a ->
  m b
rowMonadic (Colonnade v) g a =
  flip foldlMapM v $
    \e -> g (oneColonnadeEncode e a)

rowMonadic_ ::
  (Monad m) =>
  Colonnade f a c ->
  (c -> m b) ->
  a ->
  m ()
rowMonadic_ (Colonnade v) g a =
  forM_ v $ \e -> g (oneColonnadeEncode e a)

rowMonoidal ::
  (Monoid m) =>
  Colonnade h a c ->
  (c -> m) ->
  a ->
  m
rowMonoidal (Colonnade v) g a =
  foldMap (\(OneColonnade _ encode) -> g (encode a)) v

rowMonoidalHeader ::
  (Monoid m) =>
  Colonnade h a c ->
  (h c -> c -> m) ->
  a ->
  m
rowMonoidalHeader (Colonnade v) g a =
  foldMap (\(OneColonnade h encode) -> g h (encode a)) v

rowUpdateSize ::
  -- | Get size from content
  (c -> Int) ->
  MutableSizedColonnade s h a c ->
  a ->
  ST s ()
rowUpdateSize toSize (MutableSizedColonnade v mv) a =
  if MVU.length mv /= V.length v
    then error "rowMonoidalSize: vector sizes mismatched"
    else
      V.imapM_
        ( \ix (OneColonnade _ encode) ->
            MVU.modify mv (\oldSize -> max oldSize (toSize (encode a))) ix
        )
        v

headerUpdateSize ::
  (Foldable h) =>
  -- | Get size from content
  (c -> Int) ->
  MutableSizedColonnade s h a c ->
  ST s ()
headerUpdateSize toSize (MutableSizedColonnade v mv) =
  if MVU.length mv /= V.length v
    then error "rowMonoidalSize: vector sizes mismatched"
    else
      V.imapM_
        ( \ix (OneColonnade h _) ->
            MVU.modify mv (\oldSize -> max oldSize (foldl' (\sz c -> max sz (toSize c)) 0 h)) ix
        )
        v

sizeColumns ::
  (Foldable f, Foldable h) =>
  -- | Get size from content
  (c -> Int) ->
  f a ->
  Colonnade h a c ->
  Colonnade (Sized (Maybe Int) h) a c
sizeColumns toSize rows colonnade = runST $ do
  mcol <- newMutableSizedColonnade colonnade
  headerUpdateSize toSize mcol
  mapM_ (rowUpdateSize toSize mcol) rows
  freezeMutableSizedColonnade mcol

newMutableSizedColonnade :: Colonnade h a c -> ST s (MutableSizedColonnade s h a c)
newMutableSizedColonnade (Colonnade v) = do
  mv <- MVU.replicate (V.length v) 0
  return (MutableSizedColonnade v mv)

freezeMutableSizedColonnade :: MutableSizedColonnade s h a c -> ST s (Colonnade (Sized (Maybe Int) h) a c)
freezeMutableSizedColonnade (MutableSizedColonnade v mv) =
  if MVU.length mv /= V.length v
    then error "rowMonoidalSize: vector sizes mismatched"
    else do
      sizeVec <- VU.freeze mv
      return $
        Colonnade $
          V.map (\(OneColonnade h enc, sz) -> OneColonnade (Sized (Just sz) h) enc) $
            V.zip v (GV.convert sizeVec)

rowMonadicWith ::
  (Monad m) =>
  b ->
  (b -> b -> b) ->
  Colonnade f a c ->
  (c -> m b) ->
  a ->
  m b
rowMonadicWith bempty bappend (Colonnade v) g a =
  foldlM
    ( \bl e -> do
        br <- g (oneColonnadeEncode e a)
        return (bappend bl br)
    )
    bempty
    v

header :: (c1 -> c2) -> Colonnade Headed a c1 -> Vector c2
header g (Colonnade v) =
  Vector.map (g . getHeaded . oneColonnadeHead) v

{- | This function is a helper for abusing 'Foldable' to optionally
  render a header. Its future is uncertain.
-}
headerMonadicGeneral ::
  (Monad m, Monoid b, Foldable h) =>
  Colonnade h a c ->
  (c -> m b) ->
  m b
headerMonadicGeneral (Colonnade v) g =
  id $
    fmap (mconcat . Vector.toList) $
      Vector.mapM (foldlMapM g . oneColonnadeHead) v

headerMonadic ::
  (Monad m, Monoid b) =>
  Colonnade Headed a c ->
  (c -> m b) ->
  m b
headerMonadic (Colonnade v) g =
  fmap (mconcat . Vector.toList) $ Vector.mapM (g . getHeaded . oneColonnadeHead) v

headerMonadicGeneral_ ::
  (Monad m, Headedness h) =>
  Colonnade h a c ->
  (c -> m b) ->
  m ()
headerMonadicGeneral_ (Colonnade v) g = case headednessExtract of
  Nothing -> return ()
  Just f -> Vector.mapM_ (g . f . oneColonnadeHead) v

headerMonoidalGeneral ::
  (Monoid m, Foldable h) =>
  Colonnade h a c ->
  (c -> m) ->
  m
headerMonoidalGeneral (Colonnade v) g =
  foldMap (foldMap g . oneColonnadeHead) v

headerMonoidalFull ::
  (Monoid m) =>
  Colonnade h a c ->
  (h c -> m) ->
  m
headerMonoidalFull (Colonnade v) g = foldMap (g . oneColonnadeHead) v

headerMonadic_ ::
  (Monad m) =>
  Colonnade Headed a c ->
  (c -> m b) ->
  m ()
headerMonadic_ (Colonnade v) g = Vector.mapM_ (g . getHeaded . oneColonnadeHead) v

foldlMapM :: (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldlMapM f = foldlM (\b a -> fmap (mappend b) (f a)) mempty

discard :: Cornice h p a c -> Colonnade h a c
discard = go
 where
  go :: forall h p a c. Cornice h p a c -> Colonnade h a c
  go (CorniceBase c) = c
  go (CorniceCap children) = Colonnade (getColonnade . go . oneCorniceBody =<< children)

endow :: forall p a c. (c -> c -> c) -> Cornice Headed p a c -> Colonnade Headed a c
endow f x = case x of
  CorniceBase colonnade -> colonnade
  CorniceCap v -> Colonnade (V.concatMap (\(OneCornice h b) -> go h b) v)
 where
  go :: forall p'. c -> Cornice Headed p' a c -> Vector (OneColonnade Headed a c)
  go c (CorniceBase (Colonnade v)) = V.map (mapOneColonnadeHeader (f c)) v
  go c (CorniceCap v) = V.concatMap (\(OneCornice h b) -> go (f c h) b) v

uncapAnnotated ::
  forall sz p a c h.
  AnnotatedCornice sz h p a c ->
  Colonnade (Sized sz h) a c
uncapAnnotated x = case x of
  AnnotatedCorniceBase _ colonnade -> colonnade
  AnnotatedCorniceCap _ v -> Colonnade (V.concatMap (\(OneCornice _ b) -> go b) v)
 where
  go ::
    forall p'.
    AnnotatedCornice sz h p' a c ->
    Vector (OneColonnade (Sized sz h) a c)
  go (AnnotatedCorniceBase _ (Colonnade v)) = v
  go (AnnotatedCorniceCap _ v) = V.concatMap (\(OneCornice _ b) -> go b) v

annotate :: Cornice Headed p a c -> AnnotatedCornice (Maybe Int) Headed p a c
annotate = go
 where
  go :: forall p a c. Cornice Headed p a c -> AnnotatedCornice (Maybe Int) Headed p a c
  go (CorniceBase c) =
    let len = V.length (getColonnade c)
     in AnnotatedCorniceBase
          (if len > 0 then (Just len) else Nothing)
          (mapHeadedness (Sized (Just 1)) c)
  go (CorniceCap children) =
    let annChildren = fmap (mapOneCorniceBody go) children
     in AnnotatedCorniceCap
          ( ( ( V.foldl' (combineJustInt (+))
              )
                Nothing
                . V.map (size . oneCorniceBody)
            )
              annChildren
          )
          annChildren

combineJustInt :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
combineJustInt f acc el = case acc of
  Nothing -> case el of
    Nothing -> Nothing
    Just i -> Just i
  Just i -> case el of
    Nothing -> Just i
    Just j -> Just (f i j)

mapJustInt :: (Int -> Int) -> Maybe Int -> Maybe Int
mapJustInt _ Nothing = Nothing
mapJustInt f (Just i) = Just (f i)

annotateFinely ::
  (Foldable f) =>
  -- | fold function
  (Int -> Int -> Int) ->
  -- | finalize
  (Int -> Int) ->
  -- | Get size from content
  (c -> Int) ->
  f a ->
  Cornice Headed p a c ->
  AnnotatedCornice (Maybe Int) Headed p a c
annotateFinely g finish toSize xs cornice = runST $ do
  m <- newMutableSizedCornice cornice
  sizeColonnades toSize xs m
  freezeMutableSizedCornice g finish m

sizeColonnades ::
  forall f s p a c.
  (Foldable f) =>
  -- | Get size from content
  (c -> Int) ->
  f a ->
  MutableSizedCornice s p a c ->
  ST s ()
sizeColonnades toSize xs cornice = do
  goHeader cornice
  mapM_ (goRow cornice) xs
 where
  goRow :: forall p'. MutableSizedCornice s p' a c -> a -> ST s ()
  goRow (MutableSizedCorniceBase c) a = rowUpdateSize toSize c a
  goRow (MutableSizedCorniceCap children) a = mapM_ (flip goRow a . oneCorniceBody) children
  goHeader :: forall p'. MutableSizedCornice s p' a c -> ST s ()
  goHeader (MutableSizedCorniceBase c) = headerUpdateSize toSize c
  goHeader (MutableSizedCorniceCap children) = mapM_ (goHeader . oneCorniceBody) children

freezeMutableSizedCornice ::
  forall s p a c.
  -- | fold function
  (Int -> Int -> Int) ->
  -- | finalize
  (Int -> Int) ->
  MutableSizedCornice s p a c ->
  ST s (AnnotatedCornice (Maybe Int) Headed p a c)
freezeMutableSizedCornice step finish = go
 where
  go ::
    forall p' a' c'.
    MutableSizedCornice s p' a' c' ->
    ST s (AnnotatedCornice (Maybe Int) Headed p' a' c')
  go (MutableSizedCorniceBase msc) = do
    szCol <- freezeMutableSizedColonnade msc
    let sz =
          ( mapJustInt finish
              . V.foldl' (combineJustInt step) Nothing
              . V.map (sizedSize . oneColonnadeHead)
          )
            (getColonnade szCol)
    return (AnnotatedCorniceBase sz szCol)
  go (MutableSizedCorniceCap v1) = do
    v2 <- V.mapM (traverseOneCorniceBody go) v1
    let sz =
          ( mapJustInt finish
              . V.foldl' (combineJustInt step) Nothing
              . V.map (size . oneCorniceBody)
          )
            v2
    return $ AnnotatedCorniceCap sz v2

newMutableSizedCornice ::
  forall s p a c.
  Cornice Headed p a c ->
  ST s (MutableSizedCornice s p a c)
newMutableSizedCornice = go
 where
  go :: forall p'. Cornice Headed p' a c -> ST s (MutableSizedCornice s p' a c)
  go (CorniceBase c) = fmap MutableSizedCorniceBase (newMutableSizedColonnade c)
  go (CorniceCap v) = fmap MutableSizedCorniceCap (V.mapM (traverseOneCorniceBody go) v)

traverseOneCorniceBody :: (Monad m) => (k p a c -> m (j p a c)) -> OneCornice k p a c -> m (OneCornice j p a c)
traverseOneCorniceBody f (OneCornice h b) = fmap (OneCornice h) (f b)

mapHeadedness :: (forall x. h x -> h' x) -> Colonnade h a c -> Colonnade h' a c
mapHeadedness f (Colonnade v) =
  Colonnade (V.map (\(OneColonnade h c) -> OneColonnade (f h) c) v)

-- | This is an O(1) operation, sort of
size :: AnnotatedCornice sz h p a c -> sz
size x = case x of
  AnnotatedCorniceBase m _ -> m
  AnnotatedCorniceCap sz _ -> sz

mapOneCorniceBody :: (forall p' a' c'. k p' a' c' -> j p' a' c') -> OneCornice k p a c -> OneCornice j p a c
mapOneCorniceBody f (OneCornice h b) = OneCornice h (f b)

mapOneColonnadeHeader :: (Functor h) => (c -> c) -> OneColonnade h a c -> OneColonnade h a c
mapOneColonnadeHeader f (OneColonnade h b) = OneColonnade (fmap f h) b

headersMonoidal ::
  forall sz r m c p a h.
  (Monoid m, Headedness h) =>
  -- | Apply the Fascia header row content
  Maybe (Fascia p r, r -> m -> m) ->
  -- | Build content from cell content and size
  [(sz -> c -> m, m -> m)] ->
  AnnotatedCornice sz h p a c ->
  m
headersMonoidal wrapRow fromContentList = go wrapRow
 where
  go :: forall p'. Maybe (Fascia p' r, r -> m -> m) -> AnnotatedCornice sz h p' a c -> m
  go ef (AnnotatedCorniceBase _ (Colonnade v)) =
    let g :: m -> m
        g m = case ef of
          Nothing -> m
          Just (FasciaBase r, f) -> f r m
     in case headednessExtract of
          Just unhead ->
            g $
              foldMap
                ( \(fromContent, wrap) ->
                    wrap
                      ( foldMap
                          ( \(OneColonnade (Sized sz h) _) ->
                              (fromContent sz (unhead h))
                          )
                          v
                      )
                )
                fromContentList
          Nothing -> mempty
  go ef (AnnotatedCorniceCap _ v) =
    let g :: m -> m
        g m = case ef of
          Nothing -> m
          Just (FasciaCap r _, f) -> f r m
     in g
          ( foldMap
              ( \(fromContent, wrap) ->
                  wrap
                    ( foldMap
                        ( \(OneCornice h b) ->
                            (fromContent (size b) h)
                        )
                        v
                    )
              )
              fromContentList
          )
          <> case ef of
            Nothing -> case flattenAnnotated v of
              Nothing -> mempty
              Just annCoreNext -> go Nothing annCoreNext
            Just (FasciaCap _ fn, f) -> case flattenAnnotated v of
              Nothing -> mempty
              Just annCoreNext -> go (Just (fn, f)) annCoreNext

flattenAnnotated ::
  Vector (OneCornice (AnnotatedCornice sz h) p a c) ->
  Maybe (AnnotatedCornice sz h p a c)
flattenAnnotated v = case v V.!? 0 of
  Nothing -> Nothing
  Just (OneCornice _ x) -> Just $ case x of
    AnnotatedCorniceBase m _ -> flattenAnnotatedBase m v
    AnnotatedCorniceCap m _ -> flattenAnnotatedCap m v

flattenAnnotatedBase ::
  sz ->
  Vector (OneCornice (AnnotatedCornice sz h) Base a c) ->
  AnnotatedCornice sz h Base a c
flattenAnnotatedBase msz =
  AnnotatedCorniceBase msz
    . Colonnade
    . V.concatMap
      (\(OneCornice _ (AnnotatedCorniceBase _ (Colonnade v))) -> v)

flattenAnnotatedCap ::
  sz ->
  Vector (OneCornice (AnnotatedCornice sz h) (Cap p) a c) ->
  AnnotatedCornice sz h (Cap p) a c
flattenAnnotatedCap m = AnnotatedCorniceCap m . V.concatMap getTheVector

getTheVector ::
  OneCornice (AnnotatedCornice sz h) (Cap p) a c ->
  Vector (OneCornice (AnnotatedCornice sz h) p a c)
getTheVector (OneCornice _ (AnnotatedCorniceCap _ v)) = v

data MutableSizedCornice s (p :: Pillar) a c where
  MutableSizedCorniceBase ::
    {-# UNPACK #-} !(MutableSizedColonnade s Headed a c) ->
    MutableSizedCornice s Base a c
  MutableSizedCorniceCap ::
    {-# UNPACK #-} !(Vector (OneCornice (MutableSizedCornice s) p a c)) ->
    MutableSizedCornice s (Cap p) a c

data MutableSizedColonnade s h a c = MutableSizedColonnade
  { _mutableSizedColonnadeColumns :: {-# UNPACK #-} !(Vector (OneColonnade h a c))
  , _mutableSizedColonnadeSizes :: {-# UNPACK #-} !(MVU.STVector s Int)
  }

{- | As the first argument to the 'Colonnade' type
  constructor, this indictates that the columnar encoding has
  a header. This type is isomorphic to 'Identity' but is
  given a new name to clarify its intent:

> example :: Colonnade Headed Foo Text

  The term @example@ represents a columnar encoding of @Foo@
  in which the columns have headings.
-}
newtype Headed a = Headed {getHeaded :: a}
  deriving (Eq, Ord, Functor, Show, Read, Foldable)

instance Applicative Headed where
  pure = Headed
  Headed f <*> Headed a = Headed (f a)

{- | As the first argument to the 'Colonnade' type
  constructor, this indictates that the columnar encoding does not have
  a header. This type is isomorphic to 'Proxy' but is
  given a new name to clarify its intent:

> example :: Colonnade Headless Foo Text

  The term @example@ represents a columnar encoding of @Foo@
  in which the columns do not have headings.
-}
data Headless a = Headless
  deriving (Eq, Ord, Functor, Show, Read, Foldable)

instance Applicative Headless where
  pure _ = Headless
  Headless <*> Headless = Headless

data Sized sz f a = Sized
  { sizedSize :: !sz
  , sizedContent :: !(f a)
  }
  deriving (Functor, Foldable)

instance Contravariant Headless where
  contramap _ Headless = Headless

-- | Encodes a header and a cell.
data OneColonnade h a c = OneColonnade
  { oneColonnadeHead :: !(h c)
  , oneColonnadeEncode :: !(a -> c)
  }
  deriving (Functor)

instance (Functor h) => Profunctor (OneColonnade h) where
  rmap = fmap
  lmap f (OneColonnade h e) = OneColonnade h (e . f)

{- | An columnar encoding of @a@. The type variable @h@ determines what
  is present in each column in the header row. It is typically instantiated
  to 'Headed' and occasionally to 'Headless'. There is nothing that
  restricts it to these two types, although they satisfy the majority
  of use cases. The type variable @c@ is the content type. This can
  be @Text@, @String@, or @ByteString@. In the companion libraries
  @reflex-dom-colonnade@ and @yesod-colonnade@, additional types
  that represent HTML with element attributes are provided that serve
  as the content type. Presented more visually:

>             +---- Value consumed to build a row
>             |
>             v
> Colonnade h a c
>           ^   ^
>           |   |
>           |   +-- Content (Text, ByteString, Html, etc.)
>           |
>           +------ Headedness (Headed or Headless)

  Internally, a 'Colonnade' is represented as a 'Vector' of individual
  column encodings. It is possible to use any collection type with
  'Alternative' and 'Foldable' instances. However, 'Vector' was chosen to
  optimize the data structure for the use case of building the structure
  once and then folding over it many times. It is recommended that
  'Colonnade's are defined at the top-level so that GHC avoids reconstructing
  them every time they are used.
-}
newtype Colonnade h a c = Colonnade
  { getColonnade :: Vector (OneColonnade h a c)
  }
  deriving (Monoid, Functor)

instance (Functor h) => Profunctor (Colonnade h) where
  rmap = fmap
  lmap f (Colonnade v) = Colonnade (Vector.map (lmap f) v)

instance Semigroup (Colonnade h a c) where
  Colonnade a <> Colonnade b = Colonnade (a Vector.++ b)
  sconcat xs = Colonnade (vectorConcatNE (fmap getColonnade xs))

{- | Isomorphic to the natural numbers. Only the promoted version of
  this type is used.
-}
data Pillar = Cap !Pillar | Base

class ToEmptyCornice (p :: Pillar) where
  toEmptyCornice :: Cornice h p a c

instance ToEmptyCornice Base where
  toEmptyCornice = CorniceBase mempty

instance ToEmptyCornice (Cap p) where
  toEmptyCornice = CorniceCap Vector.empty

data Fascia (p :: Pillar) r where
  FasciaBase :: !r -> Fascia Base r
  FasciaCap :: !r -> Fascia p r -> Fascia (Cap p) r

data OneCornice k (p :: Pillar) a c = OneCornice
  { oneCorniceHead :: !c
  , oneCorniceBody :: !(k p a c)
  }
  deriving (Functor)

data Cornice h (p :: Pillar) a c where
  CorniceBase :: !(Colonnade h a c) -> Cornice h Base a c
  CorniceCap :: {-# UNPACK #-} !(Vector (OneCornice (Cornice h) p a c)) -> Cornice h (Cap p) a c

instance (Functor h) => Functor (Cornice h p a) where
  fmap f x = case x of
    CorniceBase c -> CorniceBase (fmap f c)
    CorniceCap c -> CorniceCap (mapVectorCornice f c)

instance (Functor h) => Profunctor (Cornice h p) where
  rmap = fmap
  lmap f x = case x of
    CorniceBase c -> CorniceBase (lmap f c)
    CorniceCap c -> CorniceCap (contramapVectorCornice f c)

instance Semigroup (Cornice h p a c) where
  CorniceBase a <> CorniceBase b = CorniceBase (mappend a b)
  CorniceCap a <> CorniceCap b = CorniceCap (a Vector.++ b)
  sconcat xs@(x :| _) = case x of
    CorniceBase _ -> CorniceBase (Colonnade (vectorConcatNE (fmap (getColonnade . getCorniceBase) xs)))
    CorniceCap _ -> CorniceCap (vectorConcatNE (fmap getCorniceCap xs))

instance (ToEmptyCornice p) => Monoid (Cornice h p a c) where
  mempty = toEmptyCornice
  mappend = (Semigroup.<>)
  mconcat xs1 = case xs1 of
    [] -> toEmptyCornice
    x : xs2 -> Semigroup.sconcat (x :| xs2)

mapVectorCornice :: (Functor h) => (c -> d) -> Vector (OneCornice (Cornice h) p a c) -> Vector (OneCornice (Cornice h) p a d)
mapVectorCornice f = V.map (fmap f)

contramapVectorCornice :: (Functor h) => (b -> a) -> Vector (OneCornice (Cornice h) p a c) -> Vector (OneCornice (Cornice h) p b c)
contramapVectorCornice f = V.map (lmapOneCornice f)

lmapOneCornice :: (Functor h) => (b -> a) -> OneCornice (Cornice h) p a c -> OneCornice (Cornice h) p b c
lmapOneCornice f (OneCornice theHead theBody) = OneCornice theHead (lmap f theBody)

getCorniceBase :: Cornice h Base a c -> Colonnade h a c
getCorniceBase (CorniceBase c) = c

getCorniceCap :: Cornice h (Cap p) a c -> Vector (OneCornice (Cornice h) p a c)
getCorniceCap (CorniceCap c) = c

data AnnotatedCornice sz h (p :: Pillar) a c where
  AnnotatedCorniceBase ::
    !sz ->
    !(Colonnade (Sized sz h) a c) ->
    AnnotatedCornice sz h Base a c
  AnnotatedCorniceCap ::
    !sz ->
    {-# UNPACK #-} !(Vector (OneCornice (AnnotatedCornice sz h) p a c)) ->
    AnnotatedCornice sz h (Cap p) a c

-- data MaybeInt = JustInt {-# UNPACK #-} !Int | NothingInt

{- | This is provided with @vector-0.12@, but we include a copy here
  for compatibility.
-}
vectorConcatNE :: NonEmpty (Vector a) -> Vector a
vectorConcatNE = Vector.concat . toList

{- | This class communicates that a container holds either zero
  elements or one element. Furthermore, all inhabitants of
  the type must hold the same number of elements. Both
  'Headed' and 'Headless' have instances. The following
  law accompanies any instances:

  > maybe x (\f -> f (headednessPure x)) headednessContents == x
  > todo: come up with another law that relates to Traversable

  Consequently, there is no instance for 'Maybe', which cannot
  satisfy the laws since it has inhabitants which hold different
  numbers of elements. 'Nothing' holds 0 elements and 'Just' holds
  1 element.
-}
class Headedness h where
  headednessPure :: a -> h a
  headednessExtract :: Maybe (h a -> a)
  headednessExtractForall :: Maybe (ExtractForall h)

instance Headedness Headed where
  headednessPure = Headed
  headednessExtract = Just getHeaded
  headednessExtractForall = Just (ExtractForall getHeaded)

instance Headedness Headless where
  headednessPure _ = Headless
  headednessExtract = Nothing
  headednessExtractForall = Nothing

newtype ExtractForall h = ExtractForall {runExtractForall :: forall a. h a -> a}
