{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Reflex.Dom

import qualified Reflex.Dom as Dom

import ReflexJsx (jsx)

main :: IO ()
main = return ()

-------------------------------------------------------------------------------
-- Some examples that should compile.
-------------------------------------------------------------------------------

-- These test basic html parsing
html1, html2, html3, html4 :: (MonadWidget t m) => m ()
html1 = [jsx|<div/>|]
html2 = [jsx|<div></div>|]
html3 = [jsx|<div>Hello</div>|]
html4 = [jsx|<h1>My <span style="color:red">Important</span> Heading</h1>|]

-- These test antiquotation (MonadWidget values + dynamic attributes)
anti1, anti2, anti3, anti4, anti5 :: (MonadWidget t m) => m ()
anti1 = [jsx|<div>{Dom.text "Hello"}</div>|]
anti2 = [jsx|<div>{Dom.text "Hello"}{Dom.text "World"}</div>|]
anti3 = let str = "Hello" in [jsx|<div>{Dom.text str}</div>|]
anti4 = [jsx|<div style={"color: " <> color <> ";width:10px;height:10px"}/>|]
  where color = "blue"
anti5 = [jsx|<span {...attrs}/>|]
  where attrs = constDyn $ "class" =: "my-class"
