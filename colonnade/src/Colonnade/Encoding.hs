-- | Build backend-agnostic columnar encodings that can be used to visualize data.

module Colonnade.Encoding
  ( -- * Example
    -- $setup
    -- * Create
    headed
  , headless
  , singleton
    -- * Transform
  , fromMaybe
  , columns
  , bool
  , replaceWhen
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
import qualified Data.Bool
import qualified Data.Maybe
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Colonnade.Internal as Internal

-- $setup
--
-- First, let\'s bring in some neccessary imports that will be
-- used for the remainder of the examples in the docs:
--
-- >>> import Data.Monoid (mconcat,(<>))
-- >>> import Data.Functor.Contravariant (contramap)
--
-- Assume that the data we wish to encode is:
--
-- >>> data Color = Red | Green | Blue deriving (Show,Eq)
-- >>> data Person = Person { name :: String, age :: Int }
-- >>> data House = House { color :: Color, price :: Int }
--
-- One potential columnar encoding of a @Person@ would be:
--
-- >>> :{
-- let encodingPerson :: Colonnade Headed String Person
--     encodingPerson = mconcat
--       [ headed "Name" name
--       , headed "Age" (show . age)
--       ]
-- :}
--
-- The type signature on @encodingPerson@ is not neccessary
-- but is included for clarity. We can feed data into this encoding
-- to build a table:
--
-- >>> let people = [Person "David" 63, Person "Ava" 34, Person "Sonia" 12]
-- >>> putStr (ascii encodingPerson people)
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
-- >>> let showDollar = (('$':) . show) :: Int -> String
-- >>> :{
-- let encodingHouse :: Colonnade Headed String House
--     encodingHouse = mconcat
--       [ headed "Color" (show . color)
--       , headed "Price" (showDollar . price)
--       ]
-- :}
--
-- >>> let houses = [House Green 170000, House Blue 115000, House Green 150000]
-- >>> putStr (ascii encodingHouse houses)
-- +-------+---------+
-- | Color | Price   |
-- +-------+---------+
-- | Green | $170000 |
-- | Blue  | $115000 |
-- | Green | $150000 |
-- +-------+---------+


-- | A single column with a header.
headed :: c -> (a -> c) -> Colonnade Headed c a
headed h = singleton (Headed h)

-- | A single column without a header.
headless :: (a -> c) -> Colonnade Headless c a
headless = singleton Headless

-- | A single column with any kind of header. This is not typically needed.
singleton :: f c -> (a -> c) -> Colonnade f c a
singleton h = Colonnade . Vector.singleton . OneColonnade h

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
-- >>> let encodingOwners :: Colonnade Headed String (Person,Maybe House)
-- >>>     encodingOwners = mconcat
-- >>>       [ contramap fst encodingPerson
-- >>>       , contramap snd (fromMaybe "" encodingHouse)
-- >>>       ]
-- >>> :}
--
-- >>> putStr (ascii encodingOwners owners)
-- +--------+-----+-------+---------+
-- | Name   | Age | Color | Price   |
-- +--------+-----+-------+---------+
-- | Jordan | 18  |       |         |
-- | Ruth   | 25  | Red   | $125000 |
-- | Sonia  | 12  | Green | $145000 |
-- +--------+-----+-------+---------+
fromMaybe :: c -> Colonnade f c a -> Colonnade f c (Maybe a)
fromMaybe c (Colonnade v) = Colonnade $ flip Vector.map v $
  \(OneColonnade h encode) -> OneColonnade h (maybe c encode)

-- | Convert a collection of @b@ values into a columnar encoding of
--   the same size. Suppose we decide to show a house\'s color
--   by putting a check mark in the column corresponding to
--   the color instead of by writing out the name of the color:
--
-- >>> let allColors = [Red,Green,Blue]
-- >>> let encColor = columns (\c1 c2 -> if c1 == c2 then "✓" else "") (Headed . show) allColors
-- >>> :t encColor
-- encColor :: Colonnade Headed [Char] Color
-- >>> let encHouse = headed "Price" (showDollar . price) <> contramap color encColor
-- >>> :t encHouse
-- encHouse :: Colonnade Headed [Char] House
-- >>> putStr (ascii encHouse houses)
-- +---------+-----+-------+------+
-- | Price   | Red | Green | Blue |
-- +---------+-----+-------+------+
-- | $170000 |     | ✓     |      |
-- | $115000 |     |       | ✓    |
-- | $150000 |     | ✓     |      |
-- +---------+-----+-------+------+
columns :: Foldable g
  => (b -> a -> c) -- ^ Cell content function
  -> (b -> f c) -- ^ Header content function
  -> g b -- ^ Basis for column encodings
  -> Colonnade f c a
columns getCell getHeader = id
  . Colonnade
  . Vector.map (\b -> OneColonnade (getHeader b) (getCell b))
  . Vector.fromList
  . toList

bool ::
     f c -- ^ Heading
  -> (a -> Bool) -- ^ Predicate
  -> (a -> c) -- ^ Contents when predicate is false
  -> (a -> c) -- ^ Contents when predicate is true
  -> Colonnade f c a
bool h p onTrue onFalse = singleton h (Data.Bool.bool <$> onFalse <*> onTrue <*> p)

replaceWhen ::
     c
  -> (a -> Bool)
  -> Colonnade f c a
  -> Colonnade f c a
replaceWhen newContent p (Colonnade v) = Colonnade
  ( Vector.map
    (\(OneColonnade h encode) -> OneColonnade h $ \a ->
      if p a then newContent else encode a
    ) v
  )

-- | 'Colonnade' is covariant in its content type. Consequently, it can be
--   mapped over. There is no standard typeclass for types that are covariant
--   in their second-to-last argument, so this function is provided for
--   situations that require this.
mapContent :: Functor f => (c1 -> c2) -> Colonnade f c1 a -> Colonnade f c2 a
mapContent f (Colonnade v) = Colonnade
  $ Vector.map (\(OneColonnade h c) -> (OneColonnade (fmap f h) (f . c))) v

-- | Consider providing a variant the produces a list
-- instead. It may allow more things to get inlined
-- in to a loop.
runRow :: (c1 -> c2) -> Colonnade f c1 a -> a -> Vector c2
runRow g (Colonnade v) a = flip Vector.map v $
  \(OneColonnade _ encode) -> g (encode a)

runBothMonadic_ :: Monad m
  => Colonnade Headed content a
  -> (content -> content -> m b)
  -> a
  -> m ()
runBothMonadic_ (Colonnade v) g a =
  forM_ v $ \(OneColonnade (Headed h) encode) -> g h (encode a)

runRowMonadic :: (Monad m, Monoid b)
              => Colonnade f content a
              -> (content -> m b)
              -> a
              -> m b
runRowMonadic (Colonnade v) g a =
  flip Internal.foldlMapM v
  $ \e -> g (oneColonnadeEncode e a)

runRowMonadic_ :: Monad m
  => Colonnade f content a
  -> (content -> m b)
  -> a
  -> m ()
runRowMonadic_ (Colonnade v) g a =
  forM_ v $ \e -> g (oneColonnadeEncode e a)

runRowMonadicWith :: (Monad m)
              => b
              -> (b -> b -> b)
              -> Colonnade f content a
              -> (content -> m b)
              -> a
              -> m b
runRowMonadicWith bempty bappend (Colonnade v) g a =
  foldlM (\bl e -> do
    br <- g (oneColonnadeEncode e a)
    return (bappend bl br)
  ) bempty v

runHeader :: (c1 -> c2) -> Colonnade Headed c1 a -> Vector c2
runHeader g (Colonnade v) =
  Vector.map (g . getHeaded . oneColonnadeHead) v

-- | This function is a helper for abusing 'Foldable' to optionally
--   render a header. Its future is uncertain.
runHeaderMonadicGeneral :: (Monad m, Monoid b, Foldable h)
  => Colonnade h content a
  -> (content -> m b)
  -> m b
runHeaderMonadicGeneral (Colonnade v) g = id
  $ fmap (mconcat . Vector.toList)
  $ Vector.mapM (Internal.foldlMapM g . oneColonnadeHead) v

runHeaderMonadic :: (Monad m, Monoid b)
                 => Colonnade Headed content a
                 -> (content -> m b)
                 -> m b
runHeaderMonadic (Colonnade v) g =
  fmap (mconcat . Vector.toList) $ Vector.mapM (g . getHeaded . oneColonnadeHead) v

runHeaderMonadicGeneral_ :: (Monad m, Monoid b, Foldable h)
  => Colonnade h content a
  -> (content -> m b)
  -> m ()
runHeaderMonadicGeneral_ (Colonnade v) g =
  Vector.mapM_ (Internal.foldlMapM g . oneColonnadeHead) v

runHeaderMonadic_ ::
     (Monad m)
  => Colonnade Headed content a
  -> (content -> m b)
  -> m ()
runHeaderMonadic_ (Colonnade v) g = Vector.mapM_ (g . getHeaded . oneColonnadeHead) v

-- | Render a collection of rows as an ascii table. The table\'s columns are
-- specified by the given 'Colonnade'. This implementation is inefficient and
-- does not provide any wrapping behavior. It is provided so that users can
-- try out @colonnade@ in ghci and so that @doctest@ can verify examples
-- code in the haddocks.
ascii :: Foldable f
  => Colonnade Headed String a -- ^ columnar encoding
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

