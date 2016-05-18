module Main where

import Text.Parsec (runParser)

import Reflex
import Reflex.Dom

import JSXParser

import qualified Data.List as List
import Control.Monad (sequence_)
import Control.Applicative ((*>))


main = mainWidget $ do
  let parseResult = runParser jsxParser () "test" "<div><span>hello sailor</span><div />One two three</div>"
  case parseResult of
    Left error -> text (show error)
    Right element -> renderJsx element


renderJsx :: MonadWidget t m => Node -> m ()
renderJsx node = do
  case node of
    Node tag children -> let renderedChildren = List.map renderJsx children
                         in el tag $ sequence_ renderedChildren
    Text content -> text content
