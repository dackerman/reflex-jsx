{-| The Quasiquoter for the reflex-jsx language

    The only import you need is "jsx", which is the quasiquoter. See the README
    for more information.
-}

{-# LANGUAGE TemplateHaskell #-}

module ReflexJsx.QQ
       ( jsx
       ) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta (parseExp)

import qualified Data.List as List
import qualified Data.Map as Map

import qualified Reflex.Dom as Dom

import ReflexJsx.Parser

import Prelude hiding (exp)

{-| Quasiquoter for jsx-like expressions

    Used like "[jsx| <div /> |]"
-}
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
    Text content -> [| Dom.text content |]
    SplicedNode name expression -> do
      let Right exp = parseExp expression
      return exp


outputNode :: String -> Attrs -> [Node] -> TH.ExpQ
outputNode tag attrs children =
  case attrs of
    StaticAttrs staticAttrs ->
      let stringAttrs = TH.listE $ List.map toStringAttr staticAttrs
      in [| Dom.elAttr tag (Map.fromList $(stringAttrs)) $(go renderedChildren) |]
    SplicedAttrs attrExpr -> do
      let Right exp = parseExp attrExpr
      [| Dom.elDynAttr tag $(return exp) $(go renderedChildren) |]
  where
    go [] = [| return () |]
    go (c:cs) = [| (,) <$> $(c) <*> $(go cs)|]
    renderedChildren = outputWidgetCode <$> children



toStringAttr :: (String, AttrValue) -> TH.ExpQ
toStringAttr (key, value) = case value of
  TextVal content -> [| (key, content) |]
  ExprVal exprString -> do
    let Right exp = parseExp exprString
    [| (key, $(return exp)) |]
