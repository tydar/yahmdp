module Main where

import Text.ParserCombinators.ReadP
import Data.Char (isPrint,isSpace, isDigit)
import Control.Monad

data Paragraph = Normal ParText
               | Pre String
               | Heading Int ParText
               | Quote [Paragraph]
               | UList ParText
               | OList ParText
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

newline :: ReadP String
newline = string "\r\n" <++ string "\n" <++ string "\r"

oneOf :: [String] -> ReadP ()
oneOf xs = do
    rest <- look
    let results = scanEach xs rest
    guard $ True `elem` results
    return ()
    where scanEach strs rest = map (scan rest) strs
          scan (r:rs) (s:ss) | s == r = scan rs ss
          scan _ [] = True
          scan _  _ = False

oneOfConsume :: [String] -> ReadP String
oneOfConsume xs = foldr1 (<++) $ map (string) xs

specials :: [String]
specials = ["*", "_", "`", "  \n", ">", "#", "+", "1.", "2.", "3.", "4."]

-- parses markdown text which ends with a particular separator
markdownText :: [String] -> ReadP String
markdownText [] = do
    consumed <- manyTill get (oneOf specials) <++ munch1 notInSpecials
    guard (consumed /= "")
    return $ consumed
    where notInSpecials c = not $ [c] `elem` specials
markdownText end = do
    consumed <- manyTill get (oneOfConsume end) 
    guard (consumed /= "")
    return $ consumed

parseMText :: [String] -> ReadP MText
parseMText end = do
    consumed <- markdownText end
    guard (consumed /= "")
    return $ MText consumed

-- SPAN PARSING STARTS HERE --
parseSpans :: ReadP ParText
parseSpans = many1 (hardbreak +++ codeSpan +++ spanAsterisk +++ spanUnderscore +++ parseMText [])

parseSpansContext :: [String] -> ReadP ParText
parseSpansContext c = many1 ((hardbreak +++ codeSpan +++ spanAsterisk +++ spanUnderscore) <++ parseMText c)

hardbreak :: ReadP MText
hardbreak = optional (whitespace) >> count 2 whitespace >> satisfy (=='\n') >> return Hardbreak

codeSpan :: ReadP MText
codeSpan = do
    sep <- string "``" <++ string "`"
    content <- markdownText [sep]
    return $ Code content 

spanAsterisk :: ReadP MText
spanAsterisk = strongAsterisk <++ emAsterisk

spanUnderscore :: ReadP MText
spanUnderscore = strongUnderscore <++ emUnderscore

emAsterisk :: ReadP MText
emAsterisk = do
    string "*"
    content <- markdownText ["*"]
    return $ Emph content

emUnderscore :: ReadP MText
emUnderscore = do
    string "_"
    content <- markdownText ["_"]
    return $ Emph content

strongAsterisk :: ReadP MText
strongAsterisk = do
    string "**"
    content <- markdownText ["**"]
    return $ Strong content

strongUnderscore :: ReadP MText
strongUnderscore = do
    string "__"
    content <- markdownText ["__"]
    return $ Strong content
-- SPAN PARSING ENDS HERE --

-- START BLOCK PARSING HERE --

blockParsers :: ReadP [Paragraph]
blockParsers = blockQuote +++ (many1 (olist +++ ulist +++ preformattedBlock +++ atxHeader +++ setext1Header +++ setext2Header <++ normalParagraph))

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

atxHeader :: ReadP Paragraph
atxHeader = do
    hashes <- munch1 (=='#')
    skipSpaces
    content <- parseSpans
    skipSpaces
    optional (munch1 (=='#'))
    optional (char '\n')
    guard $ (length hashes <= 6)
    return $ Heading (length hashes) content

setext1Header :: ReadP Paragraph
setext1Header = do
    content <- parseSpans 
    char '\n'
    munch1 (=='=')
    optional (char '\n')
    return $ Heading 1 content

setext2Header :: ReadP Paragraph
setext2Header = do
    content <- parseSpans
    char '\n'
    munch1 (=='-')
    optional (char '\n')
    return $ Heading 2 content

blockQuoteLine :: ReadP Paragraph
blockQuoteLine = do
    char '>'
    content <- blockParsers
    return $ Quote content

blockQuote :: ReadP [Paragraph]
blockQuote = many1 blockQuoteLine

{- One of the places I deviate from the official markdown implementation.
I don't allow nested lists (which is problematic, but should be eventually fixed). I intended
to allow this, but I was having trouble working with UList [Paragraph], so changed it
to UList ParText, sacrificing flexibility/compatibility. -}

ulistElem :: ReadP ParText
ulistElem = do
    char '+'
    skipSpaces
    content <- parseSpansContext ["\n", "+"]
    return $ content

ulist :: ReadP Paragraph
ulist = do
    elems <- many1 ulistElem
    return $ UList $ concat elems 

olistElem :: ReadP ParText
olistElem = do
    munch1 isDigit
    char '.'
    skipSpaces
    content <- parseSpansContext ["\n", "1.", "2.", "3.", "4.", "5.", "6.", "7.", "8.", "9.","0."]
    return $ content

olist :: ReadP Paragraph
olist = do
    elems <- many1 olistElem
    return $ OList $ concat elems
