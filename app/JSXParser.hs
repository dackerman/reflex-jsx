{-# LANGUAGE DeriveDataTypeable #-}

module JSXParser
       ( jsxParser
       , parseJsx
       , Node(..)
       ) where

import Text.Parsec (runParser, Parsec, try, eof, many, many1, between)
import Text.Parsec.Char (char, letter, noneOf, string, anyChar, alphaNum, spaces)

import Data.Typeable
import Data.Data
import qualified Data.Map as Map

import Control.Applicative ((<|>))

type Attrs = [(String, String)]

data Node = Node String Attrs [Node]
          | Text String
          | FreeVar String
           deriving (Show,Typeable,Data)


parseJsx :: Monad m => String -> m Node
parseJsx s =
  case runParser p () "" s of
    Left err -> fail $ show err
    Right e -> return e
  where p = do
          spaces
          node <- jsxElement
          spaces
          eof
          return node


jsxParser :: Parsec String u Node
jsxParser = jsxElement


jsxElement :: Parsec String u Node
jsxElement = do
  try jsxSelfClosingElement <|> jsxNormalElement


jsxSelfClosingElement :: Parsec String u Node
jsxSelfClosingElement = do
  char '<'
  name <- jsxElementName
  attrs <- jsxNodeAttrs
  string "/>"
  return (Node name attrs [])


jsxNormalElement :: Parsec String u Node
jsxNormalElement = do
  (name, attrs) <- jsxOpeningElement
  children <- many jsxChild
  jsxClosingElement name
  return (Node name attrs children)


jsxOpeningElement :: Parsec String u (String, Attrs)
jsxOpeningElement = do
  char '<'
  name <- jsxElementName
  attrs <- jsxNodeAttrs
  char '>'
  return (name, attrs)


jsxNodeAttrs :: Parsec String u Attrs
jsxNodeAttrs = do
  many jsxNodeAttr


jsxNodeAttr :: Parsec String u (String, String)
jsxNodeAttr = do
  key <- jsxAttributeName
  spaces
  char '='
  spaces
  value <- jsxQuotedValue
  spaces
  return (key, value)


jsxAttributeName :: Parsec String u String
jsxAttributeName = do
  many $ letter <|> char '-'


jsxQuotedValue :: Parsec String u String
jsxQuotedValue = do
  between (char '"') (char '"') $ many (noneOf "\"")


jsxClosingElement :: String -> Parsec String u ()
jsxClosingElement ele = do
  string "</"
  string ele
  char '>'
  return ()


jsxChild :: Parsec String u Node
jsxChild = do
  try jsxText <|> try jsxFreeVar <|> try jsxElement


jsxText :: Parsec String u Node
jsxText = do
  contents <- many1 $ noneOf "{<>}"
  return $ Text contents

jsxFreeVar :: Parsec String u Node
jsxFreeVar = do
  char '{'
  freeVar <- haskellVariableName
  char '}'
  return $ FreeVar freeVar

haskellVariableName :: Parsec String u String
haskellVariableName = do
  first <- letter
  rest <- many alphaNum
  return $ first : rest

jsxElementName :: Parsec String u String
jsxElementName = do
  jsxIdentifier


jsxIdentifier :: Parsec String u String
jsxIdentifier = do
  name <- many1 alphaNum
  spaces
  return name

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
