module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random

import Tree
import Tree.Random

main :: forall e. Eff (console :: CONSOLE, random :: RANDOM | e) Unit
main = do
  tree <- withHeight 10
  log $ showTree tree
