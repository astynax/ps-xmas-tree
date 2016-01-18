module Tree where

import Prelude
import qualified Data.Array as A
import qualified Data.String as S


data Decoration
  = Star
  | Apple
  | Tangerine
  | Plum

data Direction
  = RightToLeft
  | LeftToRight

data Elem
  = Decoration Decoration
  | Fur Direction Int

type Tree = Array (Array Elem)


showTree :: Tree -> String
showTree rows =
  let
    l = A.length rows
  in
    S.joinWith "\n" $ A.zipWith showLine (A.range (l - 1) 0) rows
  where
    showLine n es =
      let prefix = concat $ A.replicate n " "
      in concat $ A.cons prefix $ map showElem es

showElem :: Elem -> String
showElem e =
  case e of
    Decoration Star ->
      "*"
    Decoration Apple ->
      "@"
    Decoration Tangerine ->
      "o"
    Decoration Plum ->
      "O"
    Fur d n ->
      S.joinWith "" $ A.replicate n
         $ case d of
             LeftToRight -> ">"
             RightToLeft -> "<"


concat :: Array String -> String
concat = S.joinWith ""

elemLength :: Elem -> Int
elemLength (Fur _ n) = n
elemLength _ = 1
