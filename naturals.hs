{-
Paradigmes et Langages de Programmation
Haute École d’Ingénierie et de Gestion du Canton de Vaud
devoir 1 2022 - Exercice 3

author : nelson jeanrenaud
date : 29.04.2022
-}

-- Structure that holds a peano number
data Nat = Z | S Nat

instance Show Nat where
    show Z = "Z"
    show (S n) = "S(" ++ show n ++ ")"

zero :: Nat
zero = Z

succ :: Nat -> Nat
succ = S

pred :: Nat -> Nat
pred Z = Z
pred (S m) = m

-- operations

add :: Nat -> Nat -> Nat
add Z n = n
add (S m) n = S (add m n)

sub :: Nat -> Nat -> Nat
sub Z _ = Z
sub (S m) Z = S m
sub (S m) (S n) = sub m n

mul :: Nat -> Nat -> Nat
mul Z _ = Z
mul (S m) n = add n (mul m n)

power :: Nat -> Nat -> Nat
power _ Z = S Z
power m (S n) = mul m (power m n)

-- Comparisons and equality

equal :: Nat -> Nat -> Bool
equal Z Z = True
equal Z _ = False
equal (S m) Z = False
equal (S m) (S n) = equal m n

notEqual :: Nat -> Nat -> Bool
notEqual m n = not (equal m n)

lesserThan :: Nat -> Nat -> Bool
lesserThan Z _ = False
lesserThan _ Z = True
lesserThan (S m) (S n) = lesserThan m n

greaterThan :: Nat -> Nat -> Bool
greaterThan m n = not (lesserThan m n)

lesserOrEqualThan :: Nat -> Nat -> Bool
lesserOrEqualThan m n = lesserThan m n || equal m n

greaterOrEqualThan :: Nat -> Nat -> Bool
greaterOrEqualThan m n = greaterThan m n || equal m n

isZero :: Nat -> Bool
isZero Z = True
isZero _ = False

-- Conversion

intToNat :: Int -> Nat
intToNat n 
    | n <= 0 = Z
    | otherwise = S (intToNat (n - 1))

natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n
