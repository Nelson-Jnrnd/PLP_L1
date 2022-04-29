{-
Paradigmes et Langages de Programmation
Haute École d’Ingénierie et de Gestion du Canton de Vaud
devoir 1 2022 - Exercice 5

author : nelson jeanrenaud
date : 29.04.2022
-}

import System.IO
import System.Environment
import Data.Maybe
import Data.Char

-- Wolf, Goat and Cabbage problem
--
-- The problem is to move the wolf, goat and cabbage from the left bank to the right bank.


-- Represent a passenger of the boat
data Passenger = Wolf | Goat | Cabbage
    deriving (Eq, Show)

-- Position where the boat can be
data Position = LeftBank | RightBank
    deriving (Eq, Show)

-- Boat containing a passenger (the farmer is considered to always be on the boat)
data Boat = Boat {
    position :: Position,
    passenger :: Maybe Passenger
} deriving (Eq, Show)

-- State of the problem at a time T
data State = State {
    leftBank :: [Passenger],
    rightBank :: [Passenger],
    boat :: Boat
} deriving (Eq, Show)

baseState :: State
baseState = State {
    leftBank = [Wolf, Goat, Cabbage],
    rightBank = [],
    boat = Boat { position = LeftBank, passenger = Nothing }
}

goalState :: State
goalState = State {
    leftBank = [],
    rightBank = [Wolf, Goat, Cabbage],
    boat = Boat { position = RightBank, passenger = Nothing }
}

-- Returns true if the first passenger will eat the second if the farmer is not here
isEating :: Passenger -> Passenger -> Bool
isEating Wolf Goat = True
isEating Goat Cabbage = True
isEating _ _ = False

cantBeAloneWith :: Passenger -> Passenger -> Bool
cantBeAloneWith a b = isEating a b || isEating b a

canLeave :: Passenger -> [Passenger] -> Bool
canLeave _ [] = True
canLeave _ [x] = True
canLeave p bank = 
    let 
        bankWithoutP = filter (/= p) bank
        problematicPassengers = [x | x <- bankWithoutP , y <- bankWithoutP, cantBeAloneWith x y]
    in problematicPassengers == []

loadBoatWithPassenger :: Passenger -> State -> State
loadBoatWithPassenger p state
    | position (boat state) == LeftBank =
        State {
            leftBank = filter (/= p) $ leftBank state,
            rightBank = rightBank state,
            boat = Boat { position = LeftBank, passenger = Just p }
        }
    | otherwise =
        State {
            rightBank = filter (/= p) $ rightBank state,
            leftBank = leftBank state,
            boat = Boat { position = RightBank, passenger = Just p }
        }

unloadBoatWithPassenger :: State -> State
unloadBoatWithPassenger state
    | passenger (boat state) == Nothing = state
    | otherwise =
        case position (boat state) of
            LeftBank -> State {
                            leftBank = leftBank state ++ [fromJust $ passenger $ boat state],
                            rightBank = rightBank state,
                            boat = Boat { position = LeftBank, passenger = Nothing }
                        }
            RightBank -> State {
                            rightBank = rightBank state ++ [fromJust $ passenger $ boat state],
                            leftBank = leftBank state,
                            boat = Boat { position = RightBank, passenger = Nothing }
                        }

moveBoat :: State -> State
moveBoat state = case position (boat state) of
    LeftBank -> State {
            leftBank = leftBank state,
            rightBank = rightBank state,
            boat = Boat { position = RightBank, passenger = passenger $ boat state }
        }
    RightBank -> State {
            leftBank = leftBank state,
            rightBank = rightBank state,
            boat = Boat { position = LeftBank, passenger = passenger $ boat state }
        }

stringToPassenger :: [Char] -> Maybe Passenger
stringToPassenger str =
    let loweredStr = map toLower str
    in case loweredStr of
        "wolf" -> Just Wolf
        "goat" -> Just Goat
        "cabbage" -> Just Cabbage
        _ -> Nothing

displayState :: State -> IO ()
displayState state = do
    putStrLn "LeftBank bank:"
    mapM_ putStrLn $ map show $ leftBank state
    putStrLn "--\nRightBank bank:"
    mapM_ putStrLn $ map show $ rightBank state
    putStrLn "--\nBoat:"
    putStrLn $ show $ position $ boat state
    putStrLn $ show $ passenger $ boat state

displayHelp :: IO ()
displayHelp = do
        putStrLn ":p - Print current state"
        putStrLn ":l <passenger> - load boat with passenger"
        putStrLn ":u - unload boat with passenger"
        putStrLn ":m - move boat"
        putStrLn ":r - restart"
        putStrLn ":q - quit"
        putStrLn ":h - help"

playGame :: State -> IO ()
playGame state = do
    -- check if the game is over
        if state == goalState then
            putStrLn "You won!"
        else do
            input <- getLine
            if input /= []
                then do
                    let firstToken = head $ words input
                    case firstToken of
                        ":p" -> do 
                            displayState state
                            playGame state
                        ":l" -> do
                            -- get rest of input
                            let restOfInput = tail $ words input
                            -- get passenger
                            let p = stringToPassenger $ head restOfInput
                            -- check if passenger is valid
                            case p of
                                Just passenger -> do
                                    -- check if passenger can leave
                                    case position (boat state) of
                                        LeftBank -> do
                                            if canLeave passenger (leftBank state)
                                                then playGame (loadBoatWithPassenger passenger state)
                                                else do
                                                    putStrLn "This passenger can't leave"
                                                    playGame state
                                        RightBank -> do
                                            if canLeave passenger (rightBank state)
                                                then playGame (loadBoatWithPassenger passenger state)
                                                else do
                                                    putStrLn "This passenger can't leave"
                                                    playGame state
                                Nothing -> do
                                    putStrLn "Invalid passenger"
                                    playGame state
                        ":u" -> do playGame $ unloadBoatWithPassenger state
                        ":m" -> do playGame $ moveBoat state
                        ":r" -> do playGame baseState
                        ":q" -> return ()
                        ":h" -> do
                            displayHelp
                            playGame state
                        _ -> do
                            putStrLn "Invalid command"
                            playGame state
            else do displayHelp
                    playGame state


main :: IO ()
main = do
    putStrLn "Welcome to the game of Farmer and his boat"
    playGame baseState