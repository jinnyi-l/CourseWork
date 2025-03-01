-- Import the SudokuSolver module, including the required types
import SudokuSolver (Board, Solutions(..), numSolutions)
import System.IO

-- Function to split a comma-separated string into a list of integers
splitRow :: String -> [Int]
splitRow row = map read (splitByComma row)
  where
    splitByComma [] = [""]
    splitByComma (c:cs)
        | c == ','  = "" : rest
        | otherwise = (c : head rest) : tail rest
        where rest = splitByComma cs

-- Function to read and parse the Board from a file
readBoard :: FilePath -> IO Board
readBoard filePath = do
    contents <- readFile filePath
    let (n:rows) = lines contents  -- Read the first line (n value) and the rest as rows
        board = map splitRow rows  -- Convert each row into a list of Ints (representing the board)
    return board

-- Convert Solutions to a string representation
showSolutions :: Solutions -> String
showSolutions NoSolution = "NoSolution"
showSolutions UniqueSolution = "UniqueSolution"
showSolutions MultipleSolutions = "MultipleSolutions"

-- Test function that reads a file and tests the solver
testBoardFromFile :: FilePath -> IO ()
testBoardFromFile filePath = do
    board <- readBoard filePath
    let result = numSolutions board
    putStrLn $ "Result for " ++ filePath ++ ": " ++ showSolutions result

-- Main function to test multiple Boards
main :: IO ()
main = do
    -- List all the files you want to test with their full paths
    let basePath = "/Users/ljy/Desktop/functional/individual/dataset/"
    let testFiles = ["Board3.txt", "Board5.txt","Board7.txt","Board9.txt","Board2.txt"]
    mapM_ (testBoardFromFile . (basePath ++)) testFiles
