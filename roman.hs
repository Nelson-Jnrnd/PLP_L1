import Prelude hiding (lookup)
import Data.List
import Data.Tuple
import Data.Maybe
import Data.Array
--import Data.List.Split

data RomanSymbol = RomanSymbol {
    symbol :: String,
    value :: Int,
    maxRepeat :: Int
} deriving (Show, Eq)


romanSymbols :: [RomanSymbol]
romanSymbols = [
    RomanSymbol "M" 1000 3,
    RomanSymbol "CM" 900 1,
    RomanSymbol "D" 500 1,
    RomanSymbol "CD" 400 1,
    RomanSymbol "C" 100 3,
    RomanSymbol "XC" 90 1,
    RomanSymbol "L" 50 1,
    RomanSymbol "XL" 40 1,
    RomanSymbol "X" 10 3,
    RomanSymbol "IX" 9 1,
    RomanSymbol "V" 5 1,
    RomanSymbol "IV" 4 1,
    RomanSymbol "I" 1 3
    ]

stringToRomanSymbol :: String -> RomanSymbol
stringToRomanSymbol c = case find (\x -> symbol x == c) romanSymbols of
    Just x -> x
    Nothing -> error $ "Invalid roman symbol: " ++ c

integerToRomanSymbol :: Int -> RomanSymbol
integerToRomanSymbol i = case find (\x -> value x == i) romanSymbols of
    Just x -> x
    Nothing -> error $ "Invalid roman symbol: " ++ show i

biggestRomanSymbolSubstractable :: Int -> RomanSymbol
biggestRomanSymbolSubstractable n = case find (\x -> value x <= n) romanSymbols of
    Just x -> x
    Nothing -> error $ "No roman symbol small enough for: " ++ show n


integerToRomanNumeral :: Int -> String
integerToRomanNumeral 0 = ""
integerToRomanNumeral n =
    let
        toSubstract = biggestRomanSymbolSubstractable n
    in
        (symbol toSubstract) ++ (integerToRomanNumeral $ n - value toSubstract)

romanToNumber :: String -> Int
romanToNumber ls = if length (takeWhile (== 'M') ls) >= 4 then error "Number too big" else romanNumeralToInteger ls
-- todo fix (XX)
romanNumeralToInteger :: String -> Int
romanNumeralToInteger [] = 0
romanNumeralToInteger [x] = value $ stringToRomanSymbol [x]
romanNumeralToInteger (x:y:xs) = 
    if isJust (find (\z -> symbol z == [x]) romanSymbols)
        then (value $ fromJust (find (\z -> symbol z == [x,y]) romanSymbols)) + romanNumeralToInteger xs 
        else (value $ fromJust(find (\z -> symbol z == [x]) romanSymbols)) + romanNumeralToInteger (y : xs)