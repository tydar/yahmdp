<<<<<<< HEAD
module Main where

import Text.ParserCombinators.ReadP
import Control.Monad

data Paragraph = Normal ParText
               | Pre String
               | Heading Int ParText
               | Quote [Paragraph]
               | UList [Paragraph]
               | OList [Paragraph]

type ParText = [MText]

data MText = MText String
           | Emph String
           | Strong String
           | Code String
           | Hardbreak

whitespace :: ReadP Char
whitespace = satisfy ws
    where ws c = c `elem` [' ', '\t']

-- parses markdown text which ends with a particular separator
markdownText :: String -> ReadP String
markdownText end = do
    consumed <- manyTill get (string end) 
    return $ consumed

parseSpans :: ReadP MText
parseSpans = do
    rest <- look
     

hardbreak :: ReadP MText
hardbreak = whitespace >> count 2 whitespace >> satisfy (=='\n') >> return Hardbreak

codeSpan :: ReadP MText
codeSpan = do
    sep <- string "``" <++ string "`"
    content <- markdownText sep
    return $ Code content 

spanAsterisk :: ReadP MText
spanAsterisk = strongAsterisk <++ emAsterisk

spanUnderscore :: ReadP MText
spanUnderscore = strongUnderscore <++ emUnderscore

emAsterisk :: ReadP MText
emAsterisk = do
    string "*"
    content <- markdownText "*"
    return $ Emph content

emUnderscore :: ReadP MText
emUnderscore = do
    string "_"
    content <- markdownText "_"
    return $ Emph content

strongAsterisk :: ReadP MText
strongAsterisk = do
    string "**"
    content <- markdownText "**"
    return $ Strong content

strongUnderscore :: ReadP MText
strongUnderscore = do
    string "__"
    content <- markdownText "__"
    return $ Strong content
=======
import Text.ParserCombinators.ReadP
import Data.Char
import HTML

-- return :: a -> m a
-- wrap_para :: String -> Tag
parseMarkdown = readP_to_S $ markdown_parse where
    paragraph = do
        first <- satisfy (not . isSpace) 
        rest <- (munch para_char) 
        return $ wrap_para (first:rest) 
        where
            wrap_para s =  p (Empty s) []
            para_char c = or [isAlphaNum c, isSpace c, isPunctuation c, isSymbol c]
    markdown_parse = paragraph
>>>>>>> 2701c198b42d9ed1be4d9f7db63e2edc3206b901
