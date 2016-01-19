module XmasTree.Random (withHeight) where

import Prelude
import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Random (RANDOM(..), randomInt)
import qualified Data.Array as A
import Data.Array.Unsafe (unsafeIndex)

import XmasTree


type RandomElem = forall e. Eff (random :: RANDOM | e) Elem

withHeight :: forall e. Int -> Eff (random :: RANDOM | e ) Tree
withHeight 1 = pure [[Decoration Star]]
withHeight 2 = (A.snoc) <$> withHeight 1 <*> r2
  where
    r2 = (\x -> [Fur LeftToRight 1, x, Fur RightToLeft 1]) <$> rDec
withHeight n = (A.snoc) <$> withHeight (n - 1) <*> rElemsUpToLen (n * 2 - 1)


choose :: forall e a. Array a -> Eff (random :: RANDOM | e) a
choose xs = unsafeIndex xs <$> randomInt 0 (A.length xs - 1)


rDec :: RandomElem
rDec = Decoration <$> choose [Star, Apple, Tangerine, Plum]

rFur :: RandomElem
rFur = rFur_ $ choose [LeftToRight, RightToLeft]

rFur_ :: forall e.
         Eff (random :: RANDOM | e) Direction
      -> Eff (random :: RANDOM | e) Elem
rFur_ d = Fur <$> d <*> randomInt 1 3

rElemsUpToLen :: Int -> forall e. Eff (random :: RANDOM | e) (Array Elem)
rElemsUpToLen m = do
  e <- rFur_ (pure LeftToRight)
  (A.cons e) <$> go true (elemLength e)
  where
    go isDecoration l
      | isDecoration = A.cons <$> rDec <*> go false (l + 1)
      | (m - l) <= 3 = return [Fur RightToLeft (m - l)]
      | (m - l) == 4 =
        do
          (Fur d n) <- rFur
          let n' = if n < 3 then n else 2
          A.cons (Fur d n') <$> go true (l + n')
      | otherwise = rFur >>= \f -> A.cons f <$> go true (l + elemLength f)
