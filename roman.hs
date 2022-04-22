import Prelude hiding (lookup)
import Data.List
import Data.Tuple
import Data.Maybe

romanCharsMap :: [(Char, Int)]
romanCharsMap = [('I', 1), ('V', 5), ('X', 10), ('L', 50), ('C', 100), ('D', 500), ('M', 1000)]

romanCharToInt :: Char -> Int
romanCharToInt c = case (lookup c romanCharsMap) of
    Just x -> x
    Nothing -> error "Invalid roman numeral"


intToRomanChar :: Int -> Char
intToRomanChar i = case (lookup i $ map swap romanCharsMap) of
    Just x -> x
    Nothing -> error "Invalid roman numeral"

romanCharIsRepeatable :: Char -> Bool
romanCharIsRepeatable c = case (elemIndex (c, romanCharToInt c) romanCharsMap) of
    Just x -> x `mod` 2 == 0
    Nothing -> error "Invalid roman numeral"

reverseDrop :: Int -> [a] -> [a]
reverseDrop n xs = drop n (reverse xs)

romanToDecimal :: String -> Int
romanToDecimal [] = 0
romanToDecimal [x] = romanCharToInt x
romanToDecimal romanString = 
    let
        groups = group $ reverse romanString
        baseChars = head $ groups
        baseValue = romanCharToInt $ head baseChars
        modificators = if length groups > 1 then head $ drop 1 groups else []
        modificiatorsValue = if length groups > 1 then romanCharToInt $ head modificators else 0
    in
        if (length baseChars > 3)
            || ((length baseChars > 1) && not (romanCharIsRepeatable (head baseChars))) 
            || (length modificators > 2) 
            || ((length modificators > 1) && not (romanCharIsRepeatable (head modificators))) then
            error "Invalid Roman Numeral"
        else 
            if (baseValue <= modificiatorsValue) then
                (length baseChars * baseValue) + (romanToDecimal $ reverse $ reverseDrop (length baseChars) romanString)
            else
                (- length modificators * modificiatorsValue) + (length baseChars * baseValue) + (romanToDecimal $ reverse $ reverseDrop ((length modificators) + (length baseChars)) romanString)
