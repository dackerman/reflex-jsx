{-# LANGUAGE TemplateHaskell #-}

module JSXQuoter where

import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta (parseExp)

import Data.Text as T
import qualified Data.List as List
import qualified Data.Map as Map

import Reflex.Dom hiding (Widget)

import JSXParser

jsx :: QuasiQuoter
jsx = QuasiQuoter
  { quoteExp = quoteJsxExpression
  , quotePat = undefined
  , quoteDec = undefined
  , quoteType = undefined
  }


quoteJsxExpression :: String -> TH.ExpQ
quoteJsxExpression str = do
  exp <- parseJsx str
  outputWidgetCode exp


outputWidgetCode :: Node -> TH.ExpQ
outputWidgetCode node =
  case node of
    Node tag attrs children -> let renderedChildren = TH.listE $ List.map outputWidgetCode children
                                   stringAttrs = TH.listE $ List.map toStringAttr attrs
                               in [| elAttr tag (Map.fromList $(stringAttrs)) $ sequence_ $(renderedChildren) |]
    Text content -> [| text content |]
    FreeVar varName -> case parseExp varName of
      Left error -> fail error
      Right exp -> return exp


--                                     (String, String)
toStringAttr :: (String, AttrValue) -> TH.ExpQ
toStringAttr (key, value) = case value of
  TextVal content -> [| (key, content) |]
  ExprVal exprString -> case parseExp exprString of
    Left error -> fail error
    Right exp -> [| (key, $(return exp)) |]
