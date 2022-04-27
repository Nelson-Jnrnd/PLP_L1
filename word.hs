import System.Environment
import System.IO
import Text.Printf

fileColumnPadding = 20
wordColumnPadding = 5
lineColumnPadding = 5
sizeColumnPadding = 5


-- File Data Structure
data File = File {
    fileName :: String,
    nbLine :: Int,
    nbWord :: Int,
    size :: Int
} deriving (Show)

-- Extract the informations from a file and stores in a File Data Structure
readFileDetails :: FilePath -> IO File
readFileDetails path = do
  content <- loadFile path
  let nbLine = length $ lines content
  let nbWord = length $ words content
  let size = length content
  return $ File path nbLine nbWord size

-- Load file content in string
loadFile :: FilePath -> IO String
loadFile path = do
  handle <- openFile path ReadMode
  hSetEncoding handle utf8
  hGetContents handle

-- Display the informations of a file
displayFileDetails :: File -> IO ()
displayFileDetails file = do
    printf ("%-" ++ show(fileColumnPadding) ++ "s %" ++ show(wordColumnPadding) ++ "i %" ++ show(lineColumnPadding) ++ "i %" ++ show(sizeColumnPadding) ++ "i\n") name words lines filesize
    where
        name = fileName file
        words = nbWord file
        lines = nbLine file
        filesize = size file

-- Display the informations of all files
displayFilesDetails :: [File] -> IO ()
displayFilesDetails files = do
    printf ("%-" ++ show(fileColumnPadding) ++ "s %" ++ show(wordColumnPadding) ++ "s %" ++ show(lineColumnPadding) ++ "s %" ++ show(sizeColumnPadding) ++ "s\n") "file" "word" "line" "byte"
    mapM_ displayFileDetails files
    printf ("%-" ++ show(fileColumnPadding) ++ "s %" ++ show(wordColumnPadding) ++ "i %" ++ show(lineColumnPadding) ++ "i %" ++ show(sizeColumnPadding) ++ "i\n") "total" totalWords totalLines totalSizes
    where
        totalWords = sum $ map nbWord files
        totalLines = sum $ map nbLine files
        totalSizes = sum $ map size files

-- load two files given in argument
main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then putStrLn "Usage: ./main.hs <file1> <file2>"
    else do
        file1 <- readFileDetails $ head args
        file2 <- readFileDetails $ head $ tail args
        displayFilesDetails [file1, file2]