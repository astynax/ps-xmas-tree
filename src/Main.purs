module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random
import Data.Maybe
import DOM

import Data.DOM.Simple.Element
import Data.DOM.Simple.Window

import XmasTree.Random
import XmasTree.HTML

main :: forall e. Eff ( console :: CONSOLE
                      , random :: RANDOM
                      , dom :: DOM | e
                      ) Unit
main = do
  doc <- document globalWindow
  place <- querySelector "#pine-placeholder" doc
  case place of
    (Just el) ->
      toHtml <$> withHeight 25
             >>= (`setInnerHTML` el)
    _ ->
      error "Element with id=\"#pine-placeholder\" not found!"
