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
