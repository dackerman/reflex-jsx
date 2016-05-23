{-# LANGUAGE TemplateHaskell #-}

module ReflexJsx.QQ
       ( jsx
       ) where

import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta (parseExp)

import Data.Text as T
import qualified Data.List as List
import qualified Data.Map as Map

import Reflex.Dom hiding (Widget, Attrs)

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
    Node tag attrs children -> outputNode tag attrs children
    Text content -> [| text content |]
    FreeVar varName -> case parseExp varName of
      Left error -> fail error
      Right exp -> return exp


outputNode :: String -> Attrs -> [Node] -> TH.ExpQ
outputNode tag attrs children =
  let renderedChildren = TH.listE $ List.map outputWidgetCode children
  in case attrs of
    StaticAttrs staticAttrs ->
      let stringAttrs = TH.listE $ List.map toStringAttr staticAttrs
      in [| elAttr tag (Map.fromList $(stringAttrs)) $ sequence_ $(renderedChildren) |]
    SplicedAttrs attrExpr -> case parseExp attrExpr of
      Left error -> fail error
      Right exp -> [| elDynAttr tag $(return exp) $ sequence_ $(renderedChildren) |]


toStringAttr :: (String, AttrValue) -> TH.ExpQ
toStringAttr (key, value) = case value of
  TextVal content -> [| (key, content) |]
  ExprVal exprString -> case parseExp exprString of
    Left error -> fail error
    Right exp -> [| (key, $(return exp)) |]
