import System.Environment
import System.IO

-- JSON tokenizer

jsonDelimiter :: Char -> Bool
jsonDelimiter c = c == ' ' || c == '\n' || c == '\t' || c == '\r'

isJsonSpecialChar :: Char -> Bool
isJsonSpecialChar c = c == '{' || c == '}' || c == '[' || c == ']' || c == ',' || c == ':'

isJsonContent :: Char -> Bool
isJsonContent c = not (isJsonSpecialChar c) && not (jsonDelimiter c)

-- JSON parser
tokenizeJson :: String -> [String]
tokenizeJson [] = []
tokenizeJson s@(c:cs)
    | jsonDelimiter c = tokenizeJson cs
    | isJsonSpecialChar c = [c] : tokenizeJson cs
    | otherwise = 
        let 
            (token, rest) = span (isJsonContent) (s) 
        in 
            token : tokenizeJson rest

main::IO()
main = do
    args <- getArgs
    let file = head args
    json <- readFile file
    putStr $ show $ tokenizeJson json