-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module SudokuSolver(Board, Solutions(..), author, nickname, numSolutions) where
import Sudoku(Board, Solutions(..))



-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

import Data.List (transpose, minimumBy, sortBy, (\\))
import Data.Array.Unboxed (listArray, UArray, (!), bounds)
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Control.Monad (replicateM, forM_, when)
import Data.Maybe (isJust)
import Data.List (nub)


{- 
Description of implementation: 

This implementation defines a Sudoku solver with several key functionalities for validating, solving, and analyzing Sudoku boards of dynamic sizes. 
The main components include board validation, solution counting, and solving via backtracking with heuristic optimizations.
The optimization techniques used here include Minimum Remaining Values (MRV) and Least Constraining Value (LCV) to reduce the search space and improve performance.

Key component description:

(1) Board validation:
The validateBoard function checks each row, column, and block for duplicates, 
ensuring that all filled cells adhere to Sudoku rules. This validation helps identify boards with invalid configurations before attempting to solve them. 
Blocks are extracted using the getBlocks function, which divides the board based on its square root size, enabling support for both standard and larger Sudoku grids.

(2) Solution counting: 
The numSolutions function determines the uniqueness of a solution by using solveSudoku,
it returns a corresponding enum value: NoSolution, UniqueSolution, or MultipleSolutions.

(3) Solving logic: 
The solver employs a backtracking algorithm, enhanced by two heuristics: Minimum Remaining Values (MRV) and Least Constraining Value (LCV).
The MRV heuristic selects cells with the fewest possible values first, reducing potential choices and making the search more efficient. 
Meanwhile, LCV arranges values in a way that imposes the fewest restrictions on other cells, helping the solution progress more smoothly and quickly.
The solveSudoku function recursively applies these heuristics and checks if the board is complete.

(4) Helper functions
Supporting functions such as getRow, getCol, and getBlock retrieve values from specific areas of the board, aiding in constraint checking. 
getLegalValues ensures each cell's values conform to Sudoku rules, while placeValue updates the board configuration during recursion.



References:
1. Fast Sudoku solving strategies from Abhinav Sarkar’s Haskell Sudoku Solver (https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/)
2. Generalized techniques in Sudoku solving algorithms (https://en.wikipedia.org/wiki/Sudoku_solving_algorithms)
 -}

author :: String
author = "Jinyi Li"

nickname :: String
nickname = "MaybeGradeFiveSolver"

-- instance for Solutions
instance Show Solutions where
    show NoSolution = "NoSolution" --invalid board will also have this solution
    show UniqueSolution = "UniqueSolution"
    show MultipleSolutions = "MultipleSolutions"
    
    
    


-- validation function to check for duplicates in rows, columns, and blocks before solving

validateBoard :: Board -> Bool
validateBoard board = all noDuplicates rows && all noDuplicates columns && all noDuplicates blocks
  where
    rows = board
    columns = transpose board
    blocks = getBlocks board
    noDuplicates xs = let filledValues = filter (/= 0) xs in nub filledValues == filledValues



-- Get the blocks from the board

getBlocks :: Board -> [[Int]]
getBlocks board = 
  [concat [take blockSize (drop cStart (board !! r)) | r <- [rStart..rStart + blockSize - 1]]
  | rStart <- [0, blockSize..boardSize-1], cStart <- [0, blockSize..boardSize-1]]
  where
    boardSize = length board
    blockSize = round . sqrt . fromIntegral $ boardSize



-- Function to determine the number of solutions for a Sudoku board

numSolutions :: Board -> Solutions
numSolutions board
    | not (validateBoard board) = NoSolution
    | otherwise = case solveSudoku board of
                        []  -> NoSolution
                        [x] -> UniqueSolution
                        _   -> MultipleSolutions



-- Function to solve Sudoku using backtracking with MRV and LCV
--VARIANT: Each recursive call, solveSudoku fills in a cell, decreasing the number of empty cells remains until all are filled.
solveSudoku :: Board -> [Board]
solveSudoku board
    | isComplete board = [board]
    | otherwise        = concatMap solveSudoku nextBoards
  where
    (row, col) = selectMRV board
    values     = orderLCV board row col
    nextBoards = [placeValue board row col v | v <- values]




-- Check if the board is complete
isComplete :: Board -> Bool
isComplete = all (all (/= 0))



-- Select the cell with the fewest remaining values (MRV heuristic)
selectMRV :: Board -> (Int, Int)
selectMRV board =
    let emptyCells = [(r, c) | r <- [0..size-1], c <- [0..size-1], board !! r !! c == 0]
        mrvCell    = minimumBy (\a b -> compare (numLegalValues board a) (numLegalValues board b)) emptyCells
    in mrvCell
  where
    size = length board



-- Get the number of legal values for a given cell
numLegalValues :: Board -> (Int, Int) -> Int
numLegalValues board (row, col) = length (getLegalValues board row col)



-- Retrieve the legal values for a cell, applying Sudoku constraints
getLegalValues :: Board -> Int -> Int -> [Int]
getLegalValues board row col =
    let size   = length board
        blockN = round (sqrt (fromIntegral size))
        used   = getRow board row ++ getCol board col ++ getBlock board row col blockN
    in [1..size] \\ used



-- Get row by idx
getRow :: Board -> Int -> [Int]
getRow board row = board !! row

-- Get column by idx
getCol :: Board -> Int -> [Int]
getCol board col = map (!! col) board



-- Get block values by coordinates, accounting for board size
getBlock :: Board -> Int -> Int -> Int -> [Int]
getBlock board row col blockN =
    let rStart = (row `div` blockN) * blockN
        cStart = (col `div` blockN) * blockN
    in concat [take blockN (drop cStart (board !! r)) | r <- [rStart..rStart+blockN-1]]



-- Order the legal values for a given cell by the Least Constraining Value (LCV) heuristic
orderLCV :: Board -> Int -> Int -> [Int]
orderLCV board row col =
    let legalValues = getLegalValues board row col
        size        = length board
        lcvValues   = sortBy (\a b -> compare (constraints board row col a) (constraints board row col b)) legalValues
    in lcvValues



-- Count neighboring cells
-- VARIANT: Each recursive call reduces constraints by selecting a value.
constraints :: Board -> Int -> Int -> Int -> Int
constraints board row col value =
    let size   = length board
        blockN = round (sqrt (fromIntegral size))
        neighbors = [(r, c) | r <- [0..size-1], c <- [0..size-1], board !! r !! c == 0, (r == row || c == col || sameBlock row col r c blockN)]
        affected = length [() | (r, c) <- neighbors, value `elem` getLegalValues board r c]
    in affected



-- Check if two cells are in the same block
sameBlock :: Int -> Int -> Int -> Int -> Int -> Bool
sameBlock row1 col1 row2 col2 blockN =
    (row1 `div` blockN == row2 `div` blockN) && (col1 `div` blockN == col2 `div` blockN)



-- Place a value on the board
-- Variant: Each call to placeValue adds a value to the board.
placeValue :: Board -> Int -> Int -> Int -> Board
placeValue board row col value =
    let (before, rowVals:after) = splitAt row board
        (beforeCol, _:afterCol) = splitAt col rowVals
    in before ++ (beforeCol ++ [value] ++ afterCol) : after
