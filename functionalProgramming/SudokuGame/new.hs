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

-- Hardcoded board10
board10 :: Board
board10 = 
    [ [5, 3, 0, 0, 7, 0, 0, 0, 0]
    , [6, 0, 0, 1, 9, 5, 0, 0, 0]
    , [0, 9, 8, 0, 0, 0, 0, 6, 0]
    , [8, 0, 0, 0, 6, 0, 0, 0, 3]
    , [4, 0, 0, 8, 0, 3, 0, 0, 1]
    , [7, 0, 0, 0, 2, 0, 0, 0, 6]
    , [0, 6, 0, 0, 0, 0, 2, 8, 0]
    , [0, 0, 0, 4, 1, 9, 0, 0, 5]
    , [0, 0, 0, 0, 8, 0, 0, 7, 9]
    ]


-- Test function that reads a file and tests the solver
testBoardFromFile :: FilePath -> IO ()
testBoardFromFile filePath = do
    board <- readBoard filePath
    let result = numSolutions board
    putStrLn $ "Result for " ++ filePath ++ ": " ++ showSolutions result

-- Test function for hardcoded board
testBoardDirectly :: Board -> String -> IO ()
testBoardDirectly board boardName = do
    let result = numSolutions board
    putStrLn $ "Result for " ++ boardName ++ ": " ++ showSolutions result

-- Main function to test multiple Boards
main :: IO ()
main = do
    -- List all the files you want to test with their full paths
    let basePath = "/Users/ljy/Desktop/functional/individual/dataset/"
    let testFiles = ["Board3.txt", "Board5.txt", "Board7.txt", "Board9.txt", "Board2.txt"]
    
    -- Test the file-based boards
    mapM_ (testBoardFromFile . (basePath ++)) testFiles
    
    -- Test the hardcoded board10
    testBoardDirectly board10 "board10"
