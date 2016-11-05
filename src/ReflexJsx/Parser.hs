{-| The Parser for the reflex-jsx language

    Given a "String", @parseJsx@ outputs the AST for the language. Note that at
    this point, we capture spliced expressions from the meta-language as
    Strings, and parse them during the quasiquoting phase in @ReflexJsx.QQ@
-}
module ReflexJsx.Parser
       ( parseJsx
       , Node(..)
       , Attrs(..)
       , AttrValue(..)
       ) where

import Text.Parsec (runParser, Parsec, try, eof, many, many1, between)
import Text.Parsec.Char (char, anyChar, letter, noneOf, string, alphaNum, spaces)
import Text.Parsec.Combinator (manyTill)
import Debug.Trace

import Control.Applicative ((<|>))


data AttrValue = TextVal String
               | ExprVal String


data Attrs = SplicedAttrs String
           | StaticAttrs [(String, AttrValue)]


data Node = Node String Attrs [Node]
          | Text String
          | SplicedNode String String


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


jsxElement :: Parsec String u Node
jsxElement = do
  try jsxSelfClosingElement <|> jsxNormalElement


jsxSelfClosingElement :: Parsec String u Node
jsxSelfClosingElement = do
  _ <- char '<'
  name <- jsxElementName
  attrs <- jsxNodeAttrs
  _ <- string "/>"
  return (Node name attrs [])


jsxNormalElement :: Parsec String u Node
jsxNormalElement = do
  (name, attrs) <- jsxOpeningElement
  children <- many jsxChild
  jsxClosingElement name
  return (Node name attrs children)


jsxOpeningElement :: Parsec String u (String, Attrs)
jsxOpeningElement = do
  _ <- char '<'
  name <- jsxElementName
  attrs <- jsxNodeAttrs
  _ <- char '>'
  return (name, attrs)


jsxNodeAttrs :: Parsec String u Attrs
jsxNodeAttrs = do
  try jsxSplicedAttrMap <|> (StaticAttrs <$> many jsxNodeAttr)


jsxSplicedAttrMap :: Parsec String u Attrs
jsxSplicedAttrMap = do
  name <- between (string "{...") (string "}") $ many (noneOf "}")
  return $ SplicedAttrs name


jsxNodeAttr :: Parsec String u (String, AttrValue)
jsxNodeAttr = do
  key <- jsxAttributeName
  spaces
  _ <- char '='
  spaces
  value <- jsxQuotedValue <|> jsxSplicedValue
  spaces
  return (key, value)


jsxAttributeName :: Parsec String u String
jsxAttributeName = do
  many $ letter <|> char '-'


jsxQuotedValue :: Parsec String u AttrValue
jsxQuotedValue = do
  contents <- between (char '"') (char '"') $ many (noneOf "\"")
  return $ TextVal contents


jsxSplicedValue :: Parsec String u AttrValue
jsxSplicedValue = do
  name <- between (char '{') (char '}') $ many (noneOf "}")
  return $ ExprVal name


jsxClosingElement :: String -> Parsec String u ()
jsxClosingElement ele = do
  _ <- string "</" *> string ele *> char '>'
  return ()


jsxChild :: Parsec String u Node
jsxChild = do
  try jsxText <|> try jsxSplicedNode <|> try jsxElement


jsxText :: Parsec String u Node
jsxText = do
  contents <- many1 $ noneOf "{<>}"
  return $ Text contents


jsxSplicedNode :: Parsec String u Node
jsxSplicedNode = do
  name <- traceShowId <$> jsxNodeValueName
  exprString <- manyTill anyChar (try (char '}'))
  return $ SplicedNode name exprString


jsxNodeValueName :: Parsec String u String
jsxNodeValueName = between (char '{') (char '@') $ many (noneOf "@")


jsxElementName :: Parsec String u String
jsxElementName = jsxIdentifier


jsxIdentifier :: Parsec String u String
jsxIdentifier = do
  name <- many1 alphaNum
  spaces
  return name
