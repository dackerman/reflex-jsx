{-# LANGUAGE TemplateHaskell #-}

module JSXQuoter where

import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import JSXParser

jsx :: QuasiQuoter
jsx = QuasiQuoter
  { quoteExp = quoteJsxExpression
  , quotePat = undefined
  , quoteDec = undefined
  , quoteType = undefined
  }

quoteJsxExpression s = do
  exp <- parseJsx s
  dataToExpQ (const Nothing `extQ` processFreeVars) exp


processFreeVars :: Node -> Maybe TH.ExpQ
processFreeVars (FreeVar varName) =
  Just [| Text $(TH.varE (TH.mkName varName)) |]

processFreeVars _ = Nothing
