{-| The Quasiquoter for the reflex-jsx language

    The only import you need is "jsx", which is the quasiquoter. See the README
    for more information.
-}

{-# LANGUAGE TemplateHaskell #-}

module ReflexJsx.QQ
       ( jsx
       ) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Dec(DataD), newName, mkName, Type(ConT), Exp(LamE, RecConE, RecUpdE, VarE), Pat(VarP))
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
  jsx <- parseJsx str
  outputJsxCode jsx


outputJsxCode :: Jsx -> TH.ExpQ
outputJsxCode (Jsx node) = do
  --let outTypeName = newName "JsxOutput"
  --let outType = pure $ ConT outTypeName
  --let fields = lookupFields node
  --let recordTypeConstructor = RecC outTypeName (L.map makeFieldType fields)
  --addTopDecls [DataD [] outTypeName [] Nothing [recordTypeConstructor] []]
  [| \ds -> do
      results <- $(outputWidgetCode node)
      return $ foldl (\out f -> f out) ds results
   |]

--regBang = Bang NoSourceUnpackedness NoSourceStrictness
--
--makeFieldType :: (String, String) -> VarBangType
--makeFieldType (name, exp) = do
--  let Right exp = parseExp expression
--  
--
--lookupFields :: Node -> [(String, String)]
--lookupFields (Node _ _ children) = List.foldl' (++) [] $ List.map lookupFields children
--lookupFields (Text _) = []
--lookupFields (SplicedNode name expr) = [(name, expr)]

outputWidgetCode :: Node -> TH.ExpQ
outputWidgetCode node =
  case node of
    Node tag attrs children -> outputNode tag attrs children
    Text content -> [| do {Dom.text content; return []} |]
    SplicedNode name expression -> do
      let Right exp = parseExp expression
      fieldValue <- newName "x"
      lamName <- newName "b"
      let setField = LamE [VarP lamName] (RecUpdE (VarE lamName) [(mkName name, VarE fieldValue)])
      [| do { $(pure $ VarP fieldValue) <- $(pure exp); return [$(pure setField)] } |]


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
    go [] = [| return [] |]
    go (c:cs) = [| (++) <$> $(c) <*> $(go cs)|]
    renderedChildren = outputWidgetCode <$> children



toStringAttr :: (String, AttrValue) -> TH.ExpQ
toStringAttr (key, value) = case value of
  TextVal content -> [| (key, content) |]
  ExprVal exprString -> do
    let Right exp = parseExp exprString
    [| (key, $(return exp)) |]
