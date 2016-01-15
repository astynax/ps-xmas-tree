module Tree.Random (withHeight) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Random
import qualified Data.Array as A
import Data.Array.Unsafe (unsafeIndex)
import Data.Maybe (fromMaybe)

import Tree


withHeight :: forall e. Int -> Eff (random :: RANDOM | e ) Tree
withHeight 1 = pure [[Decoration Star]]
withHeight 2 =
  do
    r1 <- withHeight 1
    r2 <- row2
    return $ r1 <> r2
  where
    row2 = do
      x <- rDec
      return [[Fur LeftToRight 1, x, Fur RightToLeft 1]]


choose :: forall e. Array _ -> Eff (random :: RANDOM | e) _
choose xs = unsafeIndex xs <$> randomInt 0 (A.length xs)


rDec :: forall e. Eff (random :: RANDOM | e) Elem
rDec = Decoration <$> choose [Star, Apple, Tangerine, Plum]
