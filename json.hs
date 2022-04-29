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
import Text.Read

data JsonType = JsonString String
              | JsonNumber Double
              | JsonBool Bool
              | JsonNull
              | JsonDelimiter Char
              | JsonArray [JsonType]
              | JsonObject [(String, JsonType)]
                deriving (Show, Eq)

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split separator ys = f : (split separator (dropSeparator separator rest))
  where (f, rest) = break (== separator) ys

dropSeparator :: Eq a => a ->  [a] -> [a]
dropSeparator _ [] = []
dropSeparator separator (x:xs) = if x == separator then xs else x:xs


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
tokenToJsonType s = case readMaybe s :: Maybe Double of
    Just n -> JsonNumber n
    Nothing -> JsonString s

-- JSON parser
tokenizeJson :: String -> [JsonType]
tokenizeJson [] = []
tokenizeJson s@(c:cs)
    | jsonDelimiter c = tokenizeJson cs -- skip delimiters
    | c == '[' = (JsonArray $ map tokenToJsonType $ split ',' $ takeWhile (/= ']') cs) : (tokenizeJson $ dropWhile (/= ']') cs)
    | isJsonSpecialChar c = (JsonDelimiter c) : tokenizeJson cs -- special chars are tokens
    | isJsonContent c = let (token, rest) = span isJsonContent s in (tokenToJsonType token) : tokenizeJson rest
    | otherwise = error "invalid json"

main::IO()
main = do
    args <- getArgs
    let file = head args
    json <- readFile file
    putStr $ show $ tokenizeJson json