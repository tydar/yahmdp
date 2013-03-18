module Main where

import Text.ParserCombinators.ReadP
import Control.Monad

data Paragraph = Normal ParText
               | Pre String
               | Heading Int ParText
               | Quote [Paragraph]
               | UList [Paragraph]
               | OList [Paragraph]
    deriving (Show)

type ParText = [MText]

data MText = MText String
           | Emph String
           | Strong String
           | Code String
           | Hardbreak
    deriving (Show)

whitespace :: ReadP Char
whitespace = satisfy ws
    where ws c = c `elem` [' ', '\t']

oneOf :: [String] -> ReadP String
oneOf xs = foldr1 (<++) $ map (string) xs

specials :: [String]
specials = ["*", "_", "`", "  \n"]

-- parses markdown text which ends with a particular separator
markdownText :: String -> ReadP String
markdownText [] = do
    consumed <- manyTill get (oneOf specials)
    guard (consumed /= "")
    return $ consumed
markdownText end = do
    consumed <- manyTill get (string end) 
    guard (consumed /= "")
    return $ consumed

parseMText :: ReadP MText
parseMText = do
    consumed <- markdownText []
    guard (consumed /= "")
    return $ MText consumed

-- SPAN PARSING STARTS HERE --
parseSpans :: ReadP ParText
parseSpans = many1 (hardbreak +++ codeSpan +++ spanAsterisk +++ spanUnderscore +++ parseMText)

hardbreak :: ReadP MText
hardbreak = optional (whitespace) >> count 2 whitespace >> satisfy (=='\n') >> return Hardbreak

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
-- SPAN PARSING ENDS HERE --

-- START BLOCK PARSING HERE --

normalParagraph :: ReadP Paragraph
normalParagraph = do
    content <- parseSpans
    optional (string "\n\n")
    return $ Normal content

indentedLineSpaces :: ReadP String
indentedLineSpaces = do
    string "   "
    content <- manyTill get (char '\n')
    return content

indentedLineTab :: ReadP String
indentedLineTab = do
    char '\t'
    content <- manyTill get (char '\n')
    return content

indentedLine :: ReadP String
indentedLine = indentedLineSpaces +++ indentedLineTab

preformattedBlock :: ReadP Paragraph
preformattedBlock = do
    lines <- many1 indentedLine
    return $ Pre $ concat lines
