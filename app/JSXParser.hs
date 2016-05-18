module JSXParser
       ( jsxParser
       , Node(..)
       ) where

import Text.Parsec (Parsec, try)
import Text.Parsec.Char (char, noneOf, string)
import Text.ParserCombinators.Parsec.Char (alphaNum, spaces)
import Text.ParserCombinators.Parsec.Combinator (many1)

import Control.Applicative ((<|>), many)

data Node = Node String [Node]
          | Text String
           deriving Show

-- <div class="consumed-food">
--   <div class="line">
--     <div class="stats">
--       <span class="food-name">$name</span>
--       <div class="calories">
--         ${nutritionData (calories nutrition) "cals"}
--       </div>
--     </div>
--     ${macrosPie nutrition 50}
--   </div>
--   <div class="line">
--     <div class="macros">
--       ${nutritionData (proteinGrams nutrition) "protein (g)"}
--       ${nutritionData (fatGrams nutrition) "fat (g)"}
--       ${nutritionData (carbGrams nutrition) "carbs (g)"}
--     </div>
--   </div>
-- </div>

-- consumedFoodView :: MonadWidget t m => ConsumedFood -> m ()
-- consumedFoodView (ConsumedFood id name nutrition amount) = do
--   elClass "div" "consumed-food" $ do
--     elClass "div" "line" $ do
--       elClass "div" "stats" $ do
--         elClass "span" "food-name" $ text name
--         elClass "div" "calories" $ do
--           nutritionData (calories nutrition) "cals"
--       macrosPie nutrition 50
--     elClass "div" "line" $ do
--       elClass "div" "macros" $ do
--         nutritionData (proteinGrams nutrition) "protein (g)"
--         nutritionData (fatGrams nutrition) "fat (g)"
--         nutritionData (carbGrams nutrition) "carbs (g)"

jsxParser :: Parsec String u Node
jsxParser = jsxElement

jsxElement :: Parsec String u Node
jsxElement = do
  try jsxSelfClosingElement <|> jsxNormalElement


jsxSelfClosingElement :: Parsec String u Node
jsxSelfClosingElement = do
  char '<'
  name <- jsxElementName
  string "/>"
  return (Node name [])


jsxNormalElement :: Parsec String u Node
jsxNormalElement = do
  name <- jsxOpeningElement
  children <- many jsxChild
  jsxClosingElement name
  return (Node name children)


jsxOpeningElement :: Parsec String u String
jsxOpeningElement = do
  char '<'
  name <- jsxElementName
  char '>'
  return name


jsxClosingElement :: String -> Parsec String u ()
jsxClosingElement ele = do
  string "</"
  string ele
  char '>'
  return ()


jsxChild :: Parsec String u Node
jsxChild = do
  try jsxText <|> try jsxElement


jsxText :: Parsec String u Node
jsxText = do
  contents <- many1 $ noneOf "{<>}"
  return $ Text contents


jsxElementName :: Parsec String u String
jsxElementName = do
  jsxIdentifier


jsxIdentifier :: Parsec String u String
jsxIdentifier = do
  name <- many1 alphaNum
  spaces
  return name
