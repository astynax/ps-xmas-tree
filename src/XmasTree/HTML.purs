module XmasTree.HTML (toHtml) where

import qualified Data.String as S
import Prelude

import XmasTree

type HTML = String

toHtml :: Tree -> HTML
toHtml t = "<div class=\"pine\"><div class=\"pine-content\">"
           <> S.joinWith "" (map (row <<< map fromElem) t)
           <> "</div></div>"

fromElem :: Elem -> HTML
fromElem e =
  case e of
    Fur LeftToRight 1 -> "<span class=\"fur ltr-1\">&gt;</span>"
    Fur LeftToRight 2 -> "<span class=\"fur ltr-2\">&gt;&gt;</span>"
    Fur LeftToRight _ -> "<span class=\"fur ltr-3\">&gt;&gt;&gt;</span>"

    Fur RightToLeft 1 -> "<span class=\"fur rtl-1\">&lt;</span>"
    Fur RightToLeft 2 -> "<span class=\"fur rtl-2\">&lt;&lt;</span>"
    Fur RightToLeft _ -> "<span class=\"fur rtl-3\">&lt;&lt;&lt;</span>"

    Decoration Star ->
      "<span class=\"decoration star\">*</span>"
    Decoration Tangerine ->
      "<span class=\"decoration tangerine\">o</span>"
    Decoration Apple ->
      "<span class=\"decoration apple\">@</span>"
    Decoration Plum ->
      "<span class=\"decoration plum\">O</span>"

row :: Array HTML -> HTML
row xs = "<div class=\"row\">" <> S.joinWith "" xs <> "</div>"
