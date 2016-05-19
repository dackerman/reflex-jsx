{-# LANGUAGE  QuasiQuotes #-}

module Main where

import Text.Parsec (runParser)

import Reflex
import Reflex.Dom

import JSXParser
import JSXQuoter

import qualified Data.List as List
import qualified Data.Map as Map

import Control.Monad (sequence_)
import Control.Applicative ((*>))

main = mainWidget $ do
  let someBoundVariable = "This is bound at compile time!"
  renderJsx [jsx|
                <div class="blah" style="background-color:red">
                  <span>testing span</span>
                  <div />
                  Outside the div
                  <div style="background-color:green">{someBoundVariable}</div>
                </div>
                |]

printJsxRep :: MonadWidget t m => Node -> m ()
printJsxRep node = text $ show node

renderJsx :: MonadWidget t m => Node -> m ()
renderJsx node = do
  case node of
    Node tag attrs children -> let renderedChildren = List.map renderJsx children
                                   attributeMap = Map.fromList attrs
                               in elAttr tag attributeMap $ sequence_ renderedChildren
    Text content -> text content
