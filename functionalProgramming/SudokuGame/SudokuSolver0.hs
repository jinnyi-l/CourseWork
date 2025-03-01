-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module SudokuSolver(Board, Solutions(..), author, nickname, numSolutions) where
import Sudoku(Board, Solutions(..))
import System.IO (readFile)
import Data.List (intercalate)
-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- Description of the implementation:
This solver uses a backtracking approach to count the number of solutions to a Sudoku puzzle. The solver finds the most constrained cell (i.e., the cell with the fewest possible valid numbers), then recursively attempts to place valid numbers in that cell. The recursion stops if more than one solution is found.
This script also automates testing by reading Sudoku boards from files and processing each board.
-}

author :: String
author = "Jinyi Li"

nickname :: String
nickname = "gradefive solver" -- Replace `undefined` with your solver's nickname

{- Specification:
The `numSolutions` function takes a Sudoku `Board` and returns the number of valid solutions using the `Solutions` type (NoSolution, UniqueSolution, or MultipleSolutions).
-}
numSolutions :: Board -> Solutions
numSolutions board =
    case solve board 0 of
        0 -> NoSolution
        1 -> UniqueSolution
        _ -> MultipleSolutions

-- Solver function that counts solutions
solve :: Board -> Int -> Int
solve board count
    | count > 1 = count  -- Stop if more than one solution is found
    | otherwise = 
        case findMostConstrained board of
            Nothing -> count + 1  -- No empty cell means a solution is found
            Just (row, col) -> foldl (tryNumber row col board) count (possibleValues board (row, col))

-- Try placing a number in a cell and continue solving
tryNumber :: Int -> Int -> Board -> Int -> Int -> Int
tryNumber row col board count num
    | isValid board row col num = solve (placeNumber board row col num) count
    | otherwise = count

-- Find the most constrained empty cell
findMostConstrained :: Board -> Maybe (Int, Int)
findMostConstrained board = 
    let emptyCells = [(r, c) | r <- [0..length board - 1], c <- [0..length board - 1], board !! r !! c == 0]
        sortedCells = sortBy (\a b -> compare (length (possibleValues board a)) (length (possibleValues board b))) emptyCells
    in if null sortedCells then Nothing else Just (head sortedCells)

-- Get possible values for a specific cell
possibleValues :: Board -> (Int, Int) -> [Int]
possibleValues board (row, col) = [num | num <- [1..n^2], isValid board row col num]
  where
    n = floor . sqrt . fromIntegral . length $ board

-- Check if placing a number in a specific position is valid
isValid :: Board -> Int -> Int -> Int -> Bool
isValid board row col num =
    notElem num (getRow board row) &&
    notElem num (getCol board col) &&
    notElem num (getBox board row col)

-- Get all elements in a row
getRow :: Board -> Int -> [Int]
getRow board row = board !! row

-- Get all elements in a column
getCol :: Board -> Int -> [Int]
getCol board col = [board !! r !! col | r <- [0..length board - 1]]

-- Get all elements in the n x n subgrid containing the cell
getBox :: Board -> Int -> Int -> [Int]
getBox board row col = 
    let n = floor . sqrt . fromIntegral . length $ board
        boxRowStart = (row `div` n) * n
        boxColStart = (col `div` n) * n
    in [board !! r !! c | r <- [boxRowStart..boxRowStart + n - 1], c <- [boxColStart..boxColStart + n - 1]]

-- Place a number in the board
placeNumber :: Board -> Int -> Int -> Int -> Board
placeNumber board row col num = 
    take row board ++
    [take col (board !! row) ++ [num] ++ drop (col + 1) (board !! row)] ++
    drop (row + 1) board

-- Read a Sudoku board from a file
readBoard :: FilePath -> IO Board
readBoard filePath = do
    contents <- readFile filePath
    let (nLine:boardLines) = lines contents
        n = read nLine :: Int
        board = map (map readInt . words . filter (/= ',')) boardLines
    return board
  where
    readInt c = read [c] :: Int

-- Function to run the test cases by reading the boards from files
runTestCases :: IO ()
runTestCases = do
    let basePath = "/Users/ljy/Desktop/functional/individual/dataset/board"
        fileNames = [basePath ++ show i ++ ".txt" | i <- [0..9]]
    boards <- mapM readBoard fileNames
    let results = map numSolutions boards
    putStrLn "Results of Sudoku boards:"
    mapM_ print results

-- \/\/\/ ADD THIS MODULE \/\/\/

-- Main module to run the program
module Main where
import SudokuSolver (runTestCases)

main :: IO ()
main = runTestCases
