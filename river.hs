import System.IO
import System.Environment
import Data.Maybe
import Data.Char

-- Wolf, Goat and Cabbage problem
--
-- The problem is to move the wolf, goat and cabbage from the left bank to the right bank.

data Passenger = Wolf | Goat | Cabbage
    deriving (Eq, Show)

data Position = LeftBank | RightBank
    deriving (Eq, Show)

data Boat = Boat {
    position :: Position,
    passenger :: Maybe Passenger
} deriving (Eq, Show)

data State = State {
    leftBank :: [Passenger],
    rightBank :: [Passenger],
    boat :: Boat
} deriving (Eq, Show)

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

baseState :: State
baseState = State {
    leftBank = [Wolf, Goat, Cabbage],
    rightBank = [],
    boat = Boat { position = LeftBank, passenger = Nothing }
}

displayState :: State -> IO ()
displayState state = do
    putStrLn "LeftBank bank:"
    mapM_ putStrLn $ map show $ leftBank state
    putStrLn "RightBank bank:"
    mapM_ putStrLn $ map show $ rightBank state
    putStrLn "Boat:"
    putStrLn $ show $ position $ boat state
    putStrLn $ show $ passenger $ boat state

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
    | position (boat state) == LeftBank =
        State {
            leftBank = leftBank state ++ [fromJust $ passenger $ boat state],
            rightBank = rightBank state,
            boat = Boat { position = LeftBank, passenger = Nothing }
        }
    | otherwise =
        State {
            rightBank = rightBank state ++ [fromJust $ passenger $ boat state],
            leftBank = leftBank state,
            boat = Boat { position = RightBank, passenger = Nothing }
        }

moveBoat :: State -> State
moveBoat state
    | position (boat state) == LeftBank =
        State {
            leftBank = leftBank state,
            rightBank = rightBank state,
            boat = Boat { position = RightBank, passenger = passenger $ boat state }
        }
    | otherwise =
        State {
            leftBank = leftBank state,
            rightBank = rightBank state,
            boat = Boat { position = LeftBank, passenger = passenger $ boat state }
        } -- if for case ....


stringToPassenger :: [Char] -> Maybe Passenger
stringToPassenger str =
    let loweredStr = map toLower str
    in case loweredStr of
        "wolf" -> Just Wolf
        "goat" -> Just Goat
        "cabbage" -> Just Cabbage
        _ -> Nothing

playGame :: State -> IO ()
playGame state = do
    input <- getLine
    -- get First Token of input
    let firstToken = head $ words input
    case firstToken of
        ":p" -> do displayState state
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
            putStrLn ":p - Print current state"
            putStrLn ":l <passenger> - load boat with passenger"
            putStrLn ":u - unload boat with passenger"
            putStrLn ":m - move boat"
            putStrLn ":r - restart"
            putStrLn ":q - quit"
            putStrLn ":h - help"
            playGame state
        _ -> do
            putStrLn "Invalid input"
            playGame state