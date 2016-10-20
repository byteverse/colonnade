-- | Build backend-agnostic columnar encodings that can be used to visualize data.


module Colonnade.Encoding
  ( -- * Example
    -- $setup
    -- * Create
    headed
  , headless
    -- * Transform
  , fromMaybe
  , columns
  , mapContent
    -- * Render
  , runRow
  , runRowMonadic
  , runRowMonadic_
  , runRowMonadicWith
  , runHeader
  , runHeaderMonadic
  , runHeaderMonadic_
  , runHeaderMonadicGeneral
  , runHeaderMonadicGeneral_
  , runBothMonadic_
    -- * Ascii Table
  , ascii
  ) where

import Colonnade.Types
import Data.Vector (Vector)
import Data.Foldable
import Data.Monoid (Endo(..))
import Control.Monad
import Data.Functor.Contravariant
import qualified Data.Maybe
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Colonnade.Internal as Internal

-- $setup
--
-- Assume that the data we wish to encode is:
--
-- >>> data Color = Red | Green | Blue deriving (Show)
-- >>> data Person = Person { personName :: String, personAge :: Int }
-- >>> data House = House { houseColor :: Color, housePrice :: Int }
--
-- One potential columnar encoding of a @Person@ would be:
--
-- >>> :{
-- let encodingPerson :: Encoding Headed String Person
--     encodingPerson = mconcat
--       [ headed "Name" personName
--       , headed "Age" (show . personAge)
--       ]
-- :}
--
-- The type signature on @basicPersonEncoding@ is not neccessary
-- but is included for clarity. We can feed data into this encoding
-- to build a table:
--
-- >>> let people = [Person "David" 63, Person "Ava" 34, Person "Sonia" 12]
-- >>> putStr $ ascii encodingPerson people
-- +-------+-----+
-- | Name  | Age |
-- +-------+-----+
-- | David | 63  |
-- | Ava   | 34  |
-- | Sonia | 12  |
-- +-------+-----+
--
-- Similarly, we can build a table of houses with:
--
-- >>> :{
-- let encodingHouse :: Encoding Headed String House
--     encodingHouse = mconcat
--       [ headed "Color" (show . houseColor)
--       , headed "Price" (('$':) . show . housePrice)
--       ]
-- :}
--
-- >>> let houses = [House Green 170000, House Blue 115000]
-- >>> putStr $ ascii encodingHouse houses
-- +-------+---------+
-- | Color | Price   |
-- +-------+---------+
-- | Green | $170000 |
-- | Blue  | $115000 |
-- +-------+---------+


-- | A column with a header.
headed :: content -> (a -> content) -> Encoding Headed content a
headed h f = Encoding (Vector.singleton (OneEncoding (Headed h) f))

-- | A column without a header.
headless :: (a -> content) -> Encoding Headless content a
headless f = Encoding (Vector.singleton (OneEncoding Headless f))

-- | Lift a column over a 'Maybe'. For example, if some people
--   have houses and some do not, the data that pairs them together
--   could be represented as:
--
-- >>> :{
-- >>> let owners :: [(Person,Maybe House)]
-- >>>     owners =
-- >>>       [ (Person "Jordan" 18, Nothing)
-- >>>       , (Person "Ruth" 25, Just (House Red 125000))
-- >>>       , (Person "Sonia" 12, Just (House Green 145000))
-- >>>       ]
-- >>> :}
--
-- The column encodings defined earlier can be reused with
-- the help of 'fromMaybe':
--
-- >>> :{
-- >>> let encodingOwners :: Encoding Headed String (Person,Maybe House)
-- >>>     encodingOwners = mconcat
-- >>>       [ contramap fst encodingPerson
-- >>>       , contramap snd (fromMaybe "" encodingHouse)
-- >>>       ]
-- >>> :}
--
-- >>> putStr $ ascii encodingOwners owners
-- +--------+-----+-------+---------+
-- | Name   | Age | Color | Price   |
-- +--------+-----+-------+---------+
-- | Jordan | 18  |       |         |
-- | Ruth   | 25  | Red   | $125000 |
-- | Sonia  | 12  | Green | $145000 |
-- +--------+-----+-------+---------+
fromMaybe :: c -> Encoding f c a -> Encoding f c (Maybe a)
fromMaybe c (Encoding v) = Encoding $ flip Vector.map v $
  \(OneEncoding h encode) -> OneEncoding h (maybe c encode)

-- | Convert a 'Vector' of @b@ values into a columnar encoding of
--   the same size.
columns :: (b -> a -> c) -- ^ Cell content function
        -> (b -> f c) -- ^ Header content function
        -> Vector b -- ^ Basis for column encodings
        -> Encoding f c a
columns getCell getHeader bs =
  Encoding $ Vector.map (\b -> OneEncoding (getHeader b) (getCell b)) bs


-- | Technically, 'Encoding' is a @Bifunctor@. This maps covariantly over the
-- content type. The instance will be added once GHC8 has its next release.
mapContent :: Functor f => (c1 -> c2) -> Encoding f c1 a -> Encoding f c2 a
mapContent f (Encoding v) = Encoding
  $ Vector.map (\(OneEncoding h c) -> (OneEncoding (fmap f h) (f . c))) v

-- | Consider providing a variant the produces a list
-- instead. It may allow more things to get inlined
-- in to a loop.
runRow :: (c1 -> c2) -> Encoding f c1 a -> a -> Vector c2
runRow g (Encoding v) a = flip Vector.map v $
  \(OneEncoding _ encode) -> g (encode a)

runBothMonadic_ :: Monad m
  => Encoding Headed content a
  -> (content -> content -> m b)
  -> a
  -> m ()
runBothMonadic_ (Encoding v) g a =
  forM_ v $ \(OneEncoding (Headed h) encode) -> g h (encode a)

runRowMonadic :: (Monad m, Monoid b)
              => Encoding f content a
              -> (content -> m b)
              -> a
              -> m b
runRowMonadic (Encoding v) g a =
  flip Internal.foldlMapM v
  $ \e -> g (oneEncodingEncode e a)

runRowMonadic_ :: Monad m
  => Encoding f content a
  -> (content -> m b)
  -> a
  -> m ()
runRowMonadic_ (Encoding v) g a =
  forM_ v $ \e -> g (oneEncodingEncode e a)

runRowMonadicWith :: (Monad m)
              => b
              -> (b -> b -> b)
              -> Encoding f content a
              -> (content -> m b)
              -> a
              -> m b
runRowMonadicWith bempty bappend (Encoding v) g a =
  foldlM (\bl e -> do
    br <- g (oneEncodingEncode e a)
    return (bappend bl br)
  ) bempty v

runHeader :: (c1 -> c2) -> Encoding Headed c1 a -> Vector c2
runHeader g (Encoding v) =
  Vector.map (g . getHeaded . oneEncodingHead) v

-- | This function is a helper for abusing 'Foldable' to optionally
--   render a header. Its future is uncertain.
runHeaderMonadicGeneral :: (Monad m, Monoid b, Foldable h)
  => Encoding h content a
  -> (content -> m b)
  -> m b
runHeaderMonadicGeneral (Encoding v) g = id
  $ fmap (mconcat . Vector.toList)
  $ Vector.mapM (Internal.foldlMapM g . oneEncodingHead) v

runHeaderMonadic :: (Monad m, Monoid b)
                 => Encoding Headed content a
                 -> (content -> m b)
                 -> m b
runHeaderMonadic (Encoding v) g =
  fmap (mconcat . Vector.toList) $ Vector.mapM (g . getHeaded . oneEncodingHead) v

runHeaderMonadicGeneral_ :: (Monad m, Monoid b, Foldable h)
  => Encoding h content a
  -> (content -> m b)
  -> m ()
runHeaderMonadicGeneral_ (Encoding v) g =
  Vector.mapM_ (Internal.foldlMapM g . oneEncodingHead) v

runHeaderMonadic_ ::
     (Monad m)
  => Encoding Headed content a
  -> (content -> m b)
  -> m ()
runHeaderMonadic_ (Encoding v) g = Vector.mapM_ (g . getHeaded . oneEncodingHead) v

-- | Render a collection of rows as an ascii table. The table\'s columns are
-- specified by the given 'Encoding'. This implementation is inefficient and
-- does not provide any wrapping behavior. It is provided so that users can
-- try out @colonnade@ in ghci and so that @doctest@ can verify examples
-- code in the haddocks.
ascii :: Foldable f
  => Encoding Headed String a -- ^ columnar encoding
  -> f a -- ^ rows
  -> String
ascii enc xs =
  let theHeader :: [(Int,String)]
      theHeader = (zip (enumFrom 0) . map (\s -> " " ++ s ++ " ")) (toList (runHeader id enc))
      theBody :: [[(Int,String)]]
      theBody = map (zip (enumFrom 0) . map (\s -> " " ++ s ++ " ") . toList . runRow id enc) (toList xs)
      sizes :: [Int]
      sizes = ($ replicate (length theHeader) 1) $ appEndo $ mconcat
        [ foldMap (\(i,str) -> Endo (replaceAt i (length str))) theHeader
        , (foldMap . foldMap) (\(i,str) -> Endo (replaceAt i (length str))) theBody
        ]
      paddedHeader :: [String]
      paddedHeader = map (\(i,str) -> rightPad (atDef 1 sizes i) ' ' str) theHeader
      paddedBody :: [[String]]
      paddedBody = (map . map) (\(i,str) -> rightPad (atDef 1 sizes i) ' ' str) theBody
      divider :: String
      divider = "+" ++ join (List.intersperse "+" (map (\i -> replicate i '-') sizes)) ++ "+"
      headerStr :: String
      headerStr = "|" ++ join (List.intersperse "|" paddedHeader) ++ "|"
      bodyStr :: String
      bodyStr = List.unlines (map ((\s -> "|" ++ s ++ "|") . join . List.intersperse "|") paddedBody)
   in divider ++ "\n" ++ headerStr
              ++ "\n" ++ divider
              ++ "\n" ++ bodyStr ++ divider ++ "\n"


-- this has no effect if the index is out of bounds
replaceAt :: Ord a => Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt n v (a:as) = if n > 0
  then a : replaceAt (n - 1) v as
  else (max v a) : as

rightPad :: Int -> a -> [a] -> [a]
rightPad m a xs = take m $ xs ++ repeat a

atDef :: a -> [a] -> Int -> a
atDef def = Data.Maybe.fromMaybe def .^ atMay where
  (.^) f g x1 x2 = f (g x1 x2)
  atMay = eitherToMaybe .^ at_
  eitherToMaybe = either (const Nothing) Just
  at_ xs o | o < 0 = Left $ "index must not be negative, index=" ++ show o
           | otherwise = f o xs
      where f 0 (z:_) = Right z
            f i (_:zs) = f (i-1) zs
            f i [] = Left $ "index too large, index=" ++ show o ++ ", length=" ++ show (o-i)

