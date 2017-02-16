-- | Most users of this library do not need this module. The functions
--   here are used to build functions that apply a 'Colonnade'
--   to a collection of values, building a table from them. Ultimately, 
--   a function that applies a @Colonnade Headed MyCell a@ 
--   to data will have roughly the following type:
--
-- > myTableRenderer :: Foldable g => Colonnade Headed MyCell a -> g a -> MyContent
--
--   In the companion packages @yesod-colonnade@ and
--   @reflex-dom-colonnade@, functions with
--   similar type signatures are readily available.
--   These packages use the functions provided here
--   in the implementations of their rendering functions.
--   It is recommended that users who believe they may need
--   this module look at the source of the companion packages 
--   to see an example of how this module\'s functions are used.
--   Other backends are encouraged to use these functions
--   to build monadic or monoidal content from a 'Colonnade'.
--
--   The functions exported here take a 'Colonnade' and 
--   convert it to a fragment of content. The functions whose
--   names start with @row@ take at least a @Colonnade f c a@ and an @a@
--   value to generate a row of content. The functions whose names
--   start with @header@ need the @Colonnade f c a@ but not
--   an @a@ value since a value is not needed to build a header.
--   
module Colonnade.Encode
  ( row
  , rowMonadic
  , rowMonadic_
  , rowMonadicWith
  , rowMonoidal
  , rowMonoidalHeader
  , header
  , headerMonadic
  , headerMonadic_
  , headerMonadicGeneral
  , headerMonadicGeneral_
  , headerMonoidalGeneral
  , headerMonoidalFull
  , bothMonadic_
  , freezeMutableSizedColonnade
  , newMutableSizedColonnade
  , rowUpdateSize
  , headerUpdateSize
  , sizeColumns
  ) where

import Colonnade.Internal
import Data.Vector (Vector)
import Data.Foldable
import Control.Monad.ST (ST,runST)
import Data.Monoid
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV

-- | Consider providing a variant the produces a list
-- instead. It may allow more things to get inlined
-- in to a loop.
row :: (c1 -> c2) -> Colonnade f a c1 -> a -> Vector c2
row g (Colonnade v) a = flip Vector.map v $
  \(OneColonnade _ encode) -> g (encode a)

bothMonadic_ :: Monad m
  => Colonnade Headed a c
  -> (c -> c -> m b)
  -> a
  -> m ()
bothMonadic_ (Colonnade v) g a =
  forM_ v $ \(OneColonnade (Headed h) encode) -> g h (encode a)

rowMonadic :: 
  (Monad m, Monoid b)
  => Colonnade f a c
  -> (c -> m b)
  -> a
  -> m b
rowMonadic (Colonnade v) g a =
  flip foldlMapM v
  $ \e -> g (oneColonnadeEncode e a)

rowMonadic_ :: 
     Monad m
  => Colonnade f a c
  -> (c -> m b)
  -> a
  -> m ()
rowMonadic_ (Colonnade v) g a =
  forM_ v $ \e -> g (oneColonnadeEncode e a)

rowMonoidal ::
     Monoid m
  => Colonnade h a c
  -> (c -> m)
  -> a
  -> m
rowMonoidal (Colonnade v) g a =
  foldMap (\(OneColonnade h encode) -> g (encode a)) v

rowMonoidalHeader ::
     Monoid m
  => Colonnade h a c
  -> (h c -> c -> m)
  -> a
  -> m
rowMonoidalHeader (Colonnade v) g a =
  foldMap (\(OneColonnade h encode) -> g h (encode a)) v

rowUpdateSize ::
     (c -> Int) -- ^ Get size from content
  -> MutableSizedColonnade s h a c
  -> a
  -> ST s ()
rowUpdateSize toSize (MutableSizedColonnade v mv) a = if MVU.length mv /= V.length v
  then error "rowMonoidalSize: vector sizes mismatched"
  else V.imapM_ (\ix (OneColonnade _ encode) ->
      MVU.modify mv (\oldSize -> max oldSize (toSize (encode a))) ix
    ) v

headerUpdateSize :: Foldable h
  => (c -> Int) -- ^ Get size from content
  -> MutableSizedColonnade s h a c
  -> ST s ()
headerUpdateSize toSize (MutableSizedColonnade v mv) = if MVU.length mv /= V.length v
  then error "rowMonoidalSize: vector sizes mismatched"
  else V.imapM_ (\ix (OneColonnade h _) -> 
      MVU.modify mv (\oldSize -> max oldSize (foldl' (\sz c -> max sz (toSize c)) 0 h)) ix
    ) v

sizeColumns :: (Foldable f, Foldable h)
  => (c -> Int) -- ^ Get size from content
  -> f a
  -> Colonnade h a c
  -> Colonnade (Sized h) a c
sizeColumns toSize rows colonnade = runST $ do
  mcol <- newMutableSizedColonnade colonnade
  headerUpdateSize toSize mcol 
  mapM_ (rowUpdateSize toSize mcol) rows
  freezeMutableSizedColonnade mcol

newMutableSizedColonnade :: Colonnade h a c -> ST s (MutableSizedColonnade s h a c)
newMutableSizedColonnade (Colonnade v) = do
  mv <- MVU.replicate (V.length v) 0
  return (MutableSizedColonnade v mv)

freezeMutableSizedColonnade :: MutableSizedColonnade s h a c -> ST s (Colonnade (Sized h) a c)
freezeMutableSizedColonnade (MutableSizedColonnade v mv) =
  if MVU.length mv /= V.length v
    then error "rowMonoidalSize: vector sizes mismatched"
    else do
      sizeVec <- VU.freeze mv
      return $ Colonnade
        $ V.map (\(OneColonnade h enc,sz) -> OneColonnade (Sized sz h) enc)
        $ V.zip v (GV.convert sizeVec)

rowMonadicWith :: 
  (Monad m)
  => b
  -> (b -> b -> b)
  -> Colonnade f a c
  -> (c -> m b)
  -> a
  -> m b
rowMonadicWith bempty bappend (Colonnade v) g a =
  foldlM (\bl e -> do
    br <- g (oneColonnadeEncode e a)
    return (bappend bl br)
  ) bempty v

header :: (c1 -> c2) -> Colonnade Headed a c1 -> Vector c2
header g (Colonnade v) =
  Vector.map (g . getHeaded . oneColonnadeHead) v

-- | This function is a helper for abusing 'Foldable' to optionally
--   render a header. Its future is uncertain.
headerMonadicGeneral :: (Monad m, Monoid b, Foldable h)
  => Colonnade h a c
  -> (c -> m b)
  -> m b
headerMonadicGeneral (Colonnade v) g = id
  $ fmap (mconcat . Vector.toList)
  $ Vector.mapM (foldlMapM g . oneColonnadeHead) v

headerMonadic :: 
     (Monad m, Monoid b)
  => Colonnade Headed a c
  -> (c -> m b)
  -> m b
headerMonadic (Colonnade v) g =
  fmap (mconcat . Vector.toList) $ Vector.mapM (g . getHeaded . oneColonnadeHead) v

headerMonadicGeneral_ :: 
     (Monad m, Foldable h)
  => Colonnade h a c
  -> (c -> m b)
  -> m ()
headerMonadicGeneral_ (Colonnade v) g =
  Vector.mapM_ (mapM_ g . oneColonnadeHead) v

headerMonoidalGeneral ::
     (Monoid m, Foldable h)
  => Colonnade h a c
  -> (c -> m)
  -> m
headerMonoidalGeneral (Colonnade v) g =
  foldMap (foldMap g . oneColonnadeHead) v

headerMonoidalFull ::
     Monoid m
  => Colonnade h a c
  -> (h c -> m)
  -> m
headerMonoidalFull (Colonnade v) g = foldMap (g . oneColonnadeHead) v

headerMonadic_ ::
     (Monad m)
  => Colonnade Headed a c
  -> (c -> m b)
  -> m ()
headerMonadic_ (Colonnade v) g = Vector.mapM_ (g . getHeaded . oneColonnadeHead) v

foldlMapM :: (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldlMapM f = foldlM (\b a -> fmap (mappend b) (f a)) mempty


