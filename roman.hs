import Prelude hiding (lookup)
import Data.List
import Data.Tuple
import Data.Maybe
import Data.Array

data RomanSymbol = RomanSymbol {
    symbol :: Char,
    value :: Int,
    repeatable :: Bool
} deriving (Show, Eq)


romanSymbols :: [RomanSymbol]
romanSymbols = [
    RomanSymbol 'I' 1 True,
    RomanSymbol 'V' 5 False,
    RomanSymbol 'X' 10 True,
    RomanSymbol 'L' 50 False,
    RomanSymbol 'C' 100 True,
    RomanSymbol 'D' 500 False,
    RomanSymbol 'M' 1000 True
    ]

stringToRomanSymbol :: Char -> RomanSymbol
stringToRomanSymbol c = case find (\x -> symbol x == c) romanSymbols of
    Just x -> x
    Nothing -> error $ "Invalid roman symbol: " ++ [c]

integerToRomanSymbol :: Int -> RomanSymbol
integerToRomanSymbol i = case find (\x -> value x == i) romanSymbols of
    Just x -> x
    Nothing -> error $ "Invalid roman symbol: " ++ show i

reverseDrop :: Int -> [a] -> [a]
reverseDrop n xs = drop n (reverse xs)

romanNumeralToDecimal :: String -> Int
romanNumeralToDecimal "" = 0
romanNumeralToDecimal [a] = value $ stringToRomanSymbol a
romanNumeralToDecimal numeral =
    let
        groupsOfSymbols = group $ reverse numeral
        (firstGroup, modGroup) = (
            head groupsOfSymbols, 
            if length groupsOfSymbols > 1
                then
                    head $ drop 1 groupsOfSymbols
                else
                    []
            )
        firstSymbol = if length firstGroup > 0 then stringToRomanSymbol $ head firstGroup else error "Invalid roman numeral"
        baseValue = (value firstSymbol) * (length firstGroup)
        modSymbol = if length modGroup > 0 then stringToRomanSymbol $ head modGroup else RomanSymbol '_' 0 False
        modValue = (value modSymbol) * (length modGroup)
    in
        if length firstGroup > 3 || length modGroup > 2
            || (not (repeatable firstSymbol) && (length firstGroup > 1))
            || (not (repeatable modSymbol) && (length modGroup > 1))
            then
                error $ "Invalid roman numeral: " ++ numeral
            else
                if value firstSymbol <= value modSymbol
                    then
                        baseValue + (romanNumeralToDecimal $ reverse $ reverseDrop (length firstGroup) numeral)
                    else
                        baseValue - modValue + (romanNumeralToDecimal $ reverse $ reverseDrop ((length firstGroup) + length modGroup) numeral)