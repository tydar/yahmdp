module HTML where

import Data.List (concat, zipWith, intersperse)

data Tag = Tag {
            name :: String,
            content :: Tag,
            attributes :: [(String, String)]
        } | Empty String

instance Show Tag where
    show tag@(Tag _ _ _) = "<" ++ (name tag) ++ attr ++ ">" ++ (show $ content tag) ++ "</" ++ (name tag) ++ ">"
        where unZAttr = unzip $ attributes tag
              attr = case length (attributes tag) of
                0 -> []
                _ -> ' ':(concat $ intersperse " " $ zipWith (attrize) (fst unZAttr) (snd unZAttr))
              attrize s t = s ++ "=" ++ ('"':t) ++ "\""
    show (Empty s) = s

html = Tag "html"
head = Tag "head"
body = Tag "body"
p = Tag "p"
strong = Tag "strong"
em = Tag "em"
h1 = Tag "h1"
h2 = Tag "h2"
h3 = Tag "h3"
h4 = Tag "h4"
h5 = Tag "h5"
h6 = Tag "h6"
ol = Tag "ol"
ul = Tag "ul"
li = Tag "li"
table = Tag "table"
tr = Tag "tr"
th = Tag "th"
td = Tag "td"
span = Tag "span"
div = Tag "div"
a = Tag "a"
blockquote = Tag "blockquote"
