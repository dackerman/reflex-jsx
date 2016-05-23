{-# LANGUAGE  QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Parsec (runParser)

import Reflex.Dom

import JSXParser
import JSXQuoter

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T

import Control.Monad (sequence_)
import Control.Applicative ((*>))

main = mainWidget $ do
  let testAttribute = "red"
      innerText = elClass "div" "herp" $ do
        text "This is a widget bound at compile time!"
        i <- textInput def
        dynText $ _textInput_value i
        [jsx|<div style="color:white">this is another quasiquoted thing!!!</div>|]

  [jsx|
      <div class="blah" style="background-color:red">
        <span class={"herp" ++ "derp" ++ "snoop lion"}>testing span</span>
        <div />
        Outside the div
        <div style="background-color:blue">{innerText}</div>
      </div>
      |]
