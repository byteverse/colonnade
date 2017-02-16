{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wall -fno-warn-unused-imports -fno-warn-unticked-promoted-constructors -Werror #-}
module Colonnade.Cornice.Encode
  ( annotate
  , annotateFinely
  , size
  , endow
  , discard
  , headersMonoidal
  ) where

import Colonnade.Internal
import Data.Vector (Vector)
import Control.Monad.ST (ST,runST)
import Data.Monoid
import qualified Data.Vector as V
import qualified Colonnade.Encode as E

discard :: Cornice p a c -> Colonnade Headed a c
discard = go where
  go :: forall p a c. Cornice p a c -> Colonnade Headed a c
  go (CorniceBase c) = c
  go (CorniceCap children) = Colonnade (getColonnade . go . oneCorniceBody =<< children)

endow :: forall p a c. (c -> c -> c) -> Cornice p a c -> Colonnade Headed a c
endow f x = case x of
  CorniceBase colonnade -> colonnade
  CorniceCap v -> Colonnade (V.concatMap (\(OneCornice h b) -> go h b) v)
  where
  go :: forall p'. c -> Cornice p' a c -> Vector (OneColonnade Headed a c)
  go c (CorniceBase (Colonnade v)) = V.map (mapOneColonnadeHeader (f c)) v
  go c (CorniceCap v) = V.concatMap (\(OneCornice h b) -> go (f c h) b) v

annotate :: Cornice p a c -> AnnotatedCornice p a c
annotate = go where
  go :: forall p a c. Cornice p a c -> AnnotatedCornice p a c
  go (CorniceBase c) = let len = V.length (getColonnade c) in
    AnnotatedCorniceBase
      (if len > 0 then (Just len) else Nothing)
      (mapHeadedness (Sized 1) c)
  go (CorniceCap children) =
    let annChildren = fmap (mapOneCorniceBody go) children
     in AnnotatedCorniceCap 
          ( ( ( V.foldl' (combineJustInt (+))
              ) Nothing . V.map (size . oneCorniceBody)
            ) annChildren
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

annotateFinely :: Foldable f
  => (Int -> Int -> Int) -- ^ fold function
  -> (Int -> Int) -- ^ finalize
  -> (c -> Int) -- ^ Get size from content
  -> f a
  -> Cornice p a c 
  -> AnnotatedCornice p a c
annotateFinely g finish toSize xs cornice = runST $ do
  m <- newMutableSizedCornice cornice
  sizeColonnades toSize xs m
  freezeMutableSizedCornice g finish m

sizeColonnades :: forall f s p a c.
     Foldable f
  => (c -> Int) -- ^ Get size from content
  -> f a
  -> MutableSizedCornice s p a c 
  -> ST s ()
sizeColonnades toSize xs cornice = do
  goHeader cornice
  mapM_ (goRow cornice) xs 
  where
  goRow :: forall p'. MutableSizedCornice s p' a c -> a -> ST s ()
  goRow (MutableSizedCorniceBase c) a = E.rowUpdateSize toSize c a
  goRow (MutableSizedCorniceCap children) a = mapM_ (flip goRow a . oneCorniceBody) children
  goHeader :: forall p'. MutableSizedCornice s p' a c -> ST s ()
  goHeader (MutableSizedCorniceBase c) = E.headerUpdateSize toSize c
  goHeader (MutableSizedCorniceCap children) = mapM_ (goHeader . oneCorniceBody) children
  
freezeMutableSizedCornice :: forall s p a c.
     (Int -> Int -> Int) -- ^ fold function
  -> (Int -> Int) -- ^ finalize
  -> MutableSizedCornice s p a c 
  -> ST s (AnnotatedCornice p a c)
freezeMutableSizedCornice step finish = go
  where
  go :: forall p' a' c'. MutableSizedCornice s p' a' c' -> ST s (AnnotatedCornice p' a' c')
  go (MutableSizedCorniceBase msc) = do
    szCol <- E.freezeMutableSizedColonnade msc
    let sz = 
          ( mapJustInt finish 
          . V.foldl' (combineJustInt step) Nothing 
          . V.map (Just . sizedSize . oneColonnadeHead)
          ) (getColonnade szCol)
    return (AnnotatedCorniceBase sz szCol)
  go (MutableSizedCorniceCap v1) = do
    v2 <- V.mapM (traverseOneCorniceBody go) v1
    let sz = 
          ( mapJustInt finish 
          . V.foldl' (combineJustInt step) Nothing 
          . V.map (size . oneCorniceBody)
          ) v2
    return $ AnnotatedCorniceCap sz v2

newMutableSizedCornice :: forall s p a c.
     Cornice p a c 
  -> ST s (MutableSizedCornice s p a c)
newMutableSizedCornice = go where
  go :: forall p'. Cornice p' a c -> ST s (MutableSizedCornice s p' a c)
  go (CorniceBase c) = fmap MutableSizedCorniceBase (E.newMutableSizedColonnade c)
  go (CorniceCap v) = fmap MutableSizedCorniceCap (V.mapM (traverseOneCorniceBody go) v)
    
traverseOneCorniceBody :: Monad m => (k p a c -> m (j p a c)) -> OneCornice k p a c -> m (OneCornice j p a c)
traverseOneCorniceBody f (OneCornice h b) = fmap (OneCornice h) (f b)

mapHeadedness :: (forall x. h x -> h' x) -> Colonnade h a c -> Colonnade h' a c
mapHeadedness f (Colonnade v) = 
  Colonnade (V.map (\(OneColonnade h c) -> OneColonnade (f h) c) v)


-- | This is an O(1) operation, sort of
size :: AnnotatedCornice p a c -> Maybe Int
size x = case x of
  AnnotatedCorniceBase m _ -> m
  AnnotatedCorniceCap sz _ -> sz

mapOneCorniceBody :: (forall p' a' c'. k p' a' c' -> j p' a' c') -> OneCornice k p a c -> OneCornice j p a c
mapOneCorniceBody f (OneCornice h b) = OneCornice h (f b)

mapOneColonnadeHeader :: Functor h => (c -> c) -> OneColonnade h a c -> OneColonnade h a c
mapOneColonnadeHeader f (OneColonnade h b) = OneColonnade (fmap f h) b

headersMonoidal :: forall r m c p a.
     Monoid m
  => Either (Fascia p r, r -> m -> m) (m -> m) -- ^ Apply the Fascia header row content
  -> [Int -> c -> m] -- ^ Build content from cell content and size
  -> AnnotatedCornice p a c
  -> m
headersMonoidal wrapRow fromContentList = go wrapRow
  where
  go :: forall p'. Either (Fascia p' r, r -> m -> m) (m -> m) -> AnnotatedCornice p' a c -> m
  go ef (AnnotatedCorniceBase _ (Colonnade v)) = 
    let g :: m -> m
        g m = case ef of
          Right f -> f m
          Left (FasciaBase r, f) -> f r m
     in foldMap (\fromContent -> g 
          (foldMap (\(OneColonnade (Sized sz (Headed h)) _) -> 
            (fromContent sz h)) v)) fromContentList
  go ef (AnnotatedCorniceCap _ v) = 
    let g :: m -> m
        g m = case ef of
          Right f -> f m
          Left (FasciaCap r _, f) -> f r m
     in g (foldMap (\(OneCornice h b) -> 
          (case size b of
            Nothing -> mempty
            Just sz -> fromContent sz h)
          ) v)
          <> case ef of
               Right f -> case flattenAnnotated v of
                 Nothing -> mempty
                 Just annCoreNext -> go (Right f) annCoreNext
               Left (FasciaCap _ fn, f) -> case flattenAnnotated v of
                 Nothing -> mempty
                 Just annCoreNext -> go (Left (fn,f)) annCoreNext

flattenAnnotated :: Vector (OneCornice AnnotatedCornice p a c) -> Maybe (AnnotatedCornice p a c)
flattenAnnotated v = case v V.!? 0 of 
  Nothing -> Nothing
  Just (OneCornice _ x) -> Just $ case x of
    AnnotatedCorniceBase m _ -> flattenAnnotatedBase m v
    AnnotatedCorniceCap m _ -> flattenAnnotatedCap m v

flattenAnnotatedBase :: Maybe Int -> Vector (OneCornice AnnotatedCornice Base a c) -> AnnotatedCornice Base a c
flattenAnnotatedBase msz = AnnotatedCorniceBase msz
  . Colonnade 
  . V.concatMap 
    (\(OneCornice _ (AnnotatedCorniceBase _ (Colonnade v))) -> v)

flattenAnnotatedCap :: Maybe Int -> Vector (OneCornice AnnotatedCornice (Cap p) a c) -> AnnotatedCornice (Cap p) a c
flattenAnnotatedCap m = AnnotatedCorniceCap m . V.concatMap getTheVector

getTheVector :: OneCornice AnnotatedCornice (Cap p) a c -> Vector (OneCornice AnnotatedCornice p a c)
getTheVector (OneCornice _ (AnnotatedCorniceCap _ v)) = v


