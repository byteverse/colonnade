-- | Build HTML tables using @blaze-html@ and @colonnade@. 
module Text.Blaze.Colonnade
  ( -- * Example
    -- $example
    -- * Apply
    encodeHeadedHtmlTable
  , encodeHeadlessHtmlTable
  , encodeHeadedCellTable
  , encodeHeadlessCellTable
  , encodeTable
    -- * Cell
    -- $build
  , Cell(..)
  , htmlCell
  , stringCell
  , textCell
  , lazyTextCell
  , builderCell
    -- * Interactive
  , prettyPrintTable
    -- * Discussion
    -- $discussion
  ) where

import Text.Blaze (Attribute,(!))
import Text.Blaze.Html (Html, toHtml)
import Colonnade (Colonnade,Headed,Headless)
import Data.Text (Text)
import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.String (IsString(..))
import Data.Maybe (listToMaybe)
import Data.Char (isSpace)
import qualified Data.List as List
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Colonnade.Encode as Encode
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as TBuilder

-- $example
-- We start with a few necessary imports and some example data
-- types:
-- 
-- >>> :set -XOverloadedStrings
-- >>> import Data.Monoid (mconcat,(<>))
-- >>> import Data.Char (toLower)
-- >>> import Data.Functor.Contravariant (Contravariant(contramap))
-- >>> import Colonnade (Colonnade,Headed,Headless,headed)
-- >>> import Text.Blaze.Html (Html, toHtml, toValue)
-- >>> import qualified Colonnade as C
-- >>> import qualified Text.Blaze.Html5 as H
-- >>> data Department = Management | Sales | Engineering deriving (Show,Eq)
-- >>> data Employee = Employee { name :: String, department :: Department, age :: Int }
-- 
-- We define some employees that we will display in a table:
--
-- >>> :{
-- let employees = 
--       [ Employee "Thaddeus" Sales 34
--       , Employee "Lucia" Engineering 33
--       , Employee "Pranav" Management 57
--       ]
-- :}
-- 
-- Let's build a table that displays the name and the age
-- of an employee. Additionally, we will emphasize the names of
-- engineers using a @<strong>@ tag.
--
-- >>> :{
-- let tableEmpA :: Colonnade Headed Html Employee
--     tableEmpA = mconcat
--       [ headed "Name" $ \emp -> case department emp of
--           Engineering -> H.strong (toHtml (name emp))
--           _ -> toHtml (name emp)
--       , headed "Age" (toHtml . show . age)
--       ]
-- :}
--
-- The type signature of @tableEmpA@ is inferrable but is written
-- out for clarity in this example. Additionally, note that the first
-- argument to 'headed' is of type 'Html', so @OverloadedStrings@ is
-- necessary for the above example to compile. To avoid using this extension,
-- it is possible to instead use 'toHtml' to convert a 'String' to 'Html'.
-- Let\'s continue:
--
-- >>> let customAttrs = HA.class_ "stylish-table" <> HA.id "main-table"
-- >>> prettyPrintTable (encodeHeadedHtmlTable customAttrs tableEmpA employees)
-- <table class="stylish-table" id="main-table">
--     <thead>
--         <th>Name</th>
--         <th>Age</th>
--     </thead>
--     <tbody>
--         <tr>
--             <td>Thaddeus</td>
--             <td>34</td>
--         </tr>
--         <tr>
--             <td><strong>Lucia</strong></td>
--             <td>33</td>
--         </tr>
--         <tr>
--             <td>Pranav</td>
--             <td>57</td>
--         </tr>
--     </tbody>
-- </table>
-- 
-- Excellent. As expected, Lucia\'s name is wrapped in a @<strong>@ tag 
-- since she is an engineer.
--
-- One limitation of using 'Html' as the content
-- type of a 'Colonnade' is that we are unable to add attributes to
-- the @<td>@ and @<th>@ elements. This library provides the 'Cell' type
-- to work around this problem. A 'Cell' is just 'Html' content and a set
-- of attributes to be applied to its parent @<th>@ or @<td>@. To illustrate
-- how its use, another employee table will be built. This table will
-- contain a single column indicating the department of each employ. Each
-- cell will be assigned a class name based on the department. To start off,
-- let\'s build a table that encodes departments:
--
-- >>> :{
-- let tableDept :: Colonnade Headed Cell Department
--     tableDept = mconcat
--       [ headed "Dept." $ \d -> Cell
--           (HA.class_ (toValue (map toLower (show d))))
--           (toHtml (show d))
--       ]
-- :}
--
-- We can try it out on a list of departments. We need to use
-- 'encodeHeadedCellTable' instead of 'encodeHeadedHtmlTable':
--
-- >>> let twoDepts = [Sales,Management]
-- >>> prettyPrintTable (encodeHeadedCellTable customAttrs tableDept twoDepts)
-- <table class="stylish-table" id="main-table">
--     <thead>
--         <th>Dept.</th>
--     </thead>
--     <tbody>
--         <tr>
--             <td class="sales">Sales</td>
--         </tr>
--         <tr>
--             <td class="management">Management</td>
--         </tr>
--     </tbody>
-- </table>
-- 
-- We can take advantage of 'Colonnade'\'s 'Contravariant' instance to allow
-- this to work on 'Employee'\'s instead:
--
-- >>> :t contramap
-- contramap :: Contravariant f => (a -> b) -> f b -> f a
-- >>> let tableEmpB = contramap department tableDept
-- >>> :t tableEmpB
-- tableEmpB :: Colonnade Headed Cell Employee
-- >>> prettyPrintTable (encodeHeadedCellTable customAttrs tableEmpB employees)
-- <table class="stylish-table" id="main-table">
--     <thead>
--         <th>Dept.</th>
--     </thead>
--     <tbody>
--         <tr>
--             <td class="sales">Sales</td>
--         </tr>
--         <tr>
--             <td class="engineering">Engineering</td>
--         </tr>
--         <tr>
--             <td class="management">Management</td>
--         </tr>
--     </tbody>
-- </table>


-- $build
--
-- The 'Cell' type is used to build a 'Colonnade' that 
-- has 'Html' content inside table cells and may optionally
-- have attributes added to the @<td>@ or @<th>@ elements
-- that wrap this HTML content.

-- | The attributes that will be applied to a @<td>@ and
--   the HTML content that will go inside it.
data Cell = Cell
  { cellAttributes :: !Attribute
  , cellHtml :: !Html
  }

instance IsString Cell where
  fromString = stringCell

instance Monoid Cell where
  mempty = Cell mempty mempty
  mappend (Cell a1 c1) (Cell a2 c2) = Cell (mappend a1 a2) (mappend c1 c2)

-- | Create a 'Cell' from a 'Widget'
htmlCell :: Html -> Cell
htmlCell = Cell mempty

-- | Create a 'Cell' from a 'String'
stringCell :: String -> Cell
stringCell = htmlCell . fromString

-- | Create a 'Cell' from a 'Text'
textCell :: Text -> Cell
textCell = htmlCell . toHtml

-- | Create a 'Cell' from a lazy text
lazyTextCell :: LText.Text -> Cell
lazyTextCell = textCell . LText.toStrict

-- | Create a 'Cell' from a text builder
builderCell :: TBuilder.Builder -> Cell
builderCell = lazyTextCell . TBuilder.toLazyText

encodeTable ::
     (Foldable f, Foldable h)
  => Maybe Attribute -- ^ Attributes of @<thead>@, pass 'Nothing' to omit @<thead>@
  -> Attribute -- ^ Attributes of @<tbody>@ element
  -> (a -> Attribute) -- ^ Attributes of each @<tr>@ element
  -> ((Html -> Html) -> c -> Html) -- ^ Wrap content and convert to 'Html'
  -> Attribute -- ^ Attributes of @<table>@ element
  -> Colonnade h c a -- ^ How to encode data as a row
  -> f a -- ^ Collection of data
  -> Html
encodeTable mtheadAttrs tbodyAttrs trAttrs wrapContent tableAttrs colonnade xs =
  H.table ! tableAttrs $ do
    for_ mtheadAttrs $ \theadAttrs -> do
      H.thead ! theadAttrs $ do
        Encode.headerMonoidalGeneral colonnade (wrapContent H.th)
    H.tbody ! tbodyAttrs $ do
      forM_ xs $ \x -> do
        H.tr ! trAttrs x $ Encode.rowMonoidal colonnade (wrapContent H.td) x

encodeHeadedCellTable :: 
     Foldable f
  => Attribute -- ^ Attributes of @<table>@ element
  -> Colonnade Headed Cell a -- ^ How to encode data as columns
  -> f a -- ^ Collection of data
  -> Html
encodeHeadedCellTable = encodeTable
  (Just mempty) mempty (const mempty) htmlFromCell 

encodeHeadlessCellTable :: 
     Foldable f
  => Attribute -- ^ Attributes of @<table>@ element
  -> Colonnade Headless Cell a -- ^ How to encode data as columns
  -> f a -- ^ Collection of data
  -> Html
encodeHeadlessCellTable = encodeTable
  Nothing mempty (const mempty) htmlFromCell 

encodeHeadedHtmlTable :: 
     Foldable f
  => Attribute -- ^ Attributes of @<table>@ element
  -> Colonnade Headed Html a -- ^ How to encode data as columns
  -> f a -- ^ Collection of data
  -> Html
encodeHeadedHtmlTable = encodeTable
  (Just mempty) mempty (const mempty) ($) 

encodeHeadlessHtmlTable :: 
     Foldable f
  => Attribute -- ^ Attributes of @<table>@ element
  -> Colonnade Headless Html a -- ^ How to encode data as columns
  -> f a -- ^ Collection of data
  -> Html
encodeHeadlessHtmlTable = encodeTable
  Nothing mempty (const mempty) ($) 

htmlFromCell :: (Html -> Html) -> Cell -> Html
htmlFromCell f (Cell attr content) = f ! attr $ content

data St = St
  { stContext :: [String]
  , stTagStatus :: TagStatus
  , stResult :: String -> String -- ^ difference list
  }

data TagStatus 
  = TagStatusSomeTag 
  | TagStatusOpening (String -> String)
  | TagStatusOpeningAttrs 
  | TagStatusNormal 
  | TagStatusClosing (String -> String)
  | TagStatusAfterTag

removeWhitespaceAfterTag :: String -> String -> String
removeWhitespaceAfterTag chosenTag = 
  either id (\st -> stResult st "") . foldlM (flip f) (St [] TagStatusNormal id)
  where
  f :: Char -> St -> Either String St
  f c (St ctx status res) = case status of
    TagStatusNormal
      | c == '<' -> Right (St ctx TagStatusSomeTag likelyRes)
      | isSpace c -> if Just chosenTag == listToMaybe ctx
          then Right (St ctx TagStatusNormal res) -- drops the whitespace
          else Right (St ctx TagStatusNormal likelyRes)
      | otherwise -> Right (St ctx TagStatusNormal likelyRes)
    TagStatusSomeTag
      | c == '/' -> Right (St ctx (TagStatusClosing id) likelyRes)
      | c == '>' -> Left "unexpected >"
      | c == '<' -> Left "unexpected <"
      | otherwise -> Right (St ctx (TagStatusOpening (c:)) likelyRes)
    TagStatusOpening tag
      | c == '>' -> Right (St (tag "" : ctx) TagStatusAfterTag likelyRes)
      | isSpace c -> Right (St (tag "" : ctx) TagStatusOpeningAttrs likelyRes)
      | otherwise -> Right (St ctx (TagStatusOpening (tag . (c:))) likelyRes)
    TagStatusOpeningAttrs
      | c == '>' -> Right (St ctx TagStatusAfterTag likelyRes)
      | otherwise -> Right (St ctx TagStatusOpeningAttrs likelyRes)
    TagStatusClosing tag
      | c == '>' -> do
          otherTags <- case ctx of
            [] -> Left "closing tag without any opening tag"
            closestTag : otherTags -> if closestTag == tag ""
              then Right otherTags
              else Left $ "closing tag <" ++ tag "" ++ "> did not match opening tag <" ++ closestTag ++ ">"
          Right (St otherTags TagStatusAfterTag likelyRes)
      | otherwise -> Right (St ctx (TagStatusClosing (tag . (c:))) likelyRes)
    TagStatusAfterTag
      | c == '<' -> Right (St ctx TagStatusSomeTag likelyRes)
      | isSpace c -> if Just chosenTag == listToMaybe ctx
          then Right (St ctx TagStatusAfterTag res) -- drops the whitespace
          else Right (St ctx TagStatusNormal likelyRes)
      | otherwise -> Right (St ctx TagStatusNormal likelyRes)
    where 
    likelyRes :: String -> String
    likelyRes = res . (c:)

prettyPrintTable :: Html -> IO ()
prettyPrintTable = putStrLn 
  . List.dropWhileEnd (== '\n')
  . removeWhitespaceAfterTag "td" 
  . removeWhitespaceAfterTag "th" 
  . removeWhitespaceAfterTag "strong" 
  . Pretty.renderHtml


-- $discussion
--
-- In this module, some of the functions for applying a 'Colonnade' to
-- some values to build a table have roughly this type signature:
--
-- > Foldable a => Colonnade Headedness Cell a -> f a -> Html
--
-- The 'Colonnade'\'s content type is 'Cell', but the content
-- type of the result is 'Html'. It may not be immidiately clear why
-- this is useful done. Another strategy, which this library also
-- uses, is to write
-- these functions to take a 'Colonnade' whose content is 'Html':
--
-- > Foldable a => Colonnade Headedness Html a -> f a -> Html
--
-- When the 'Colonnade'\'s content type is 'Html', then the header
-- content is rendered as the child of a @<th>@ and the row
-- content the child of a @<td>@. However, it is not possible
-- to add attributes to these parent elements. To accomodate this
-- situation, it is necessary to introduce 'Cell', which includes
-- the possibility of attributes on the parent node.


