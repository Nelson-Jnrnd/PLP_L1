{-
Paradigmes et Langages de Programmation
Haute École d’Ingénierie et de Gestion du Canton de Vaud
devoir 1 2022 - Exercice 4

author : nelson jeanrenaud
date : 29.04.2022
-}
import System.Environment
import System.IO
import Data.Char

data JsonType = JsonString String
              | JsonNumber Double
              | JsonBool Bool
              | JsonNull
              | JsonDelimiter Char
                deriving (Show, Eq)
-- JSON tokenizer

jsonDelimiter :: Char -> Bool
jsonDelimiter c = c == ' ' || c == '\n' || c == '\t' || c == '\r'

isJsonSpecialChar :: Char -> Bool
isJsonSpecialChar c = c == '{' || c == '}' || c == '[' || c == ']' || c == ',' || c == ':'

isJsonContent :: Char -> Bool
isJsonContent c = not (isJsonSpecialChar c) && not (jsonDelimiter c)

tokenToJsonType :: String -> JsonType
tokenToJsonType "null" = JsonNull
tokenToJsonType "true" = JsonBool True
tokenToJsonType "false" = JsonBool False
tokenToJsonType s@(x:xs) 
    | isNumber x = JsonNumber (read s :: Double)
    | otherwise = JsonString s

-- JSON parser
tokenizeJson :: String -> [JsonType]
tokenizeJson [] = []
tokenizeJson s@(c:cs)
    | jsonDelimiter c = tokenizeJson cs -- skip delimiters
    | isJsonSpecialChar c = (JsonDelimiter c) : tokenizeJson cs -- special chars are tokens
    | otherwise = 
        let 
            (token, rest) = span (isJsonContent) (s)  -- content is kept in one token
        in 
            (tokenToJsonType token) : tokenizeJson rest

main::IO()
main = do
    args <- getArgs
    let file = head args
    json <- readFile file
    putStr $ show $ tokenizeJson json