-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module SudokuSolver(Board, Solutions(..), author, nickname, numSolutions) where
import Sudoku(Board, Solutions(..))
import Data.List (transpose)
import Data.Array.Unboxed (listArray, UArray, (!))
import Data.STRef.Lazy
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Control.Monad.ST.Lazy
import Control.Monad (replicateM, forM_, when)
import System.IO

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- Brief description:
   This implementation solves Sudoku puzzles of dynamic sizes using the Dancing Links (DLX) algorithm.
   It reads a Sudoku puzzle from a file and counts the number of valid solutions.
   The DLX algorithm is optimized to stop once more than one solution is found.
 -}

author :: String
author = "Jinyi Li"

nickname :: String
nickname = "DLXSolver"

-- Eq instance for DNode
instance Eq (DNode s) where
    Null == Null = True
    -- Compares references of DNode objects
    a == b = False  -- You can customize this further if needed

-- Show instance for Solutions
instance Show Solutions where
    show NoSolution = "NoSolution"
    show UniqueSolution = "UniqueSolution"
    show MultipleSolutions = "MultipleSolutions"

-- Function Specification:
-- numSolutions :: Board -> Solutions
-- Given a Sudoku puzzle (Board), this function checks if there is a unique solution,
-- more than one solution, or no solution at all. It returns the appropriate Solutions type.

numSolutions :: Board -> Solutions
numSolutions prob = case runST (solve bitmap colN) of
                        [] -> NoSolution
                        [x] -> UniqueSolution
                        xs -> MultipleSolutions
                        
    where sudokuN = round $ sqrt $ fromIntegral (length prob)  -- dynamically infer `sudokuN`
          blockN = sudokuN ^ 2
          secN = blockN ^ 2
          colN = secN * 4
          fixed = filter (\(_, _, d) -> d /= 0) $
                    concatMap (\(r, l) -> (zipWith (\c x -> (r, c, x)) [0..]) l) (zip [0..] prob)
          fixedPos = map (\(a, b, _) -> (a, b)) fixed
          all = fixed ++ [(x, y, d) | x <- [0..blockN - 1], y <- [0..blockN - 1],
                                      d <- [1..blockN], (x, y) `notElem` fixedPos]
          makeBits [] n = replicate n False
          makeBits (x:xs) n = replicate x False ++ (True:makeBits (map (\t -> t - x - 1) xs) (n - x - 1))
          bitmap = [listArray (0, colN - 1) $
                        makeBits [x * blockN + y,
                                  secN + ((x `div` sudokuN) * sudokuN + y `div` sudokuN) * blockN + d - 1,
                                  secN * 2 + x * blockN + d - 1,
                                  secN * 3 + y * blockN + d - 1] colN | (x, y, d) <- all]

     
                               
-- Define the DNode data type
data DNode s = DNode { left :: STRef s (Maybe (DNode s))
                     , right :: STRef s (Maybe (DNode s))
                     , up :: STRef s (Maybe (DNode s))
                     , down :: STRef s (Maybe (DNode s))
                     , ctl :: STRef s (DNode s)
                     , size :: STRef s Int
                     , row :: STRef s Int
                     }|Null
                     

-- Create new DNode
newDNode :: Maybe (DNode s) -> Maybe (DNode s) -> Maybe (DNode s) -> Maybe (DNode s) -> Int -> Int -> Int -> ST s (DNode s)
newDNode l r u d s i j = do
    leftRef <- newSTRef l
    rightRef <- newSTRef r
    upRef <- newSTRef u
    downRef <- newSTRef d
    ctlRef <- newSTRef Null  -- or some other initialization based on your needs
    sizeRef <- newSTRef s
    rowRef <- newSTRef i
    return $ DNode leftRef rightRef upRef downRef ctlRef sizeRef rowRef


getAttr :: (DNode s -> STRef s a) -> DNode s -> ST s a
getAttr dir node = readSTRef (dir node)

setAttr :: (DNode s -> STRef s a) -> DNode s -> a -> ST s ()
setAttr dir node val = writeSTRef (dir node) val


-- DLX functions and helper functions
buildDLX :: [UArray Int Bool] -> Int -> Int -> ST s (DNode s)
buildDLX bitmap nrow ncol = do
    chead <- newArray (0, ncol - 1) Null :: ST s (STArray s Int (DNode s))
    h <- newDNode (Just Null) (Just Null) (Just Null) (Just Null) 0 (-1) (-1)
    setAttr left h (Just h)
    setAttr right h (Just h)
    setAttr up h (Just h)
    setAttr down h (Just h)
    forM_ [0..ncol-1] $ \j -> do
        hl <- getAttr left h
        case hl of
          Just hl' -> do
            p <- newDNode (Just hl') (Just h) (Just Null) (Just Null) 0 (-1) j
            setAttr right hl' (Just p)
            setAttr left h (Just p)
            setAttr up p (Just p)
            setAttr down p (Just p)
            writeArray chead j p
          Nothing -> return ()  -- Handle Nothing case for hl
    rhead <- newDNode (Just Null) (Just Null) (Just Null) (Just Null) 0 0 (-1)
    forM_ (zip [0..nrow-1] bitmap) $ \(i, row) -> do
        setAttr left rhead (Just rhead)
        setAttr right rhead (Just rhead)
        forM_ [0..ncol-1] $ \j -> do
            if row ! j then do
                rl <- getAttr left rhead
                case rl of
                  Just rl' -> do
                    ct <- readArray chead j
                    cs <- getAttr size ct
                    setAttr size ct (cs + 1)
                    cu <- getAttr up ct
                    case cu of
                        Just cu' -> do
                            p <- newDNode (Just rl') (Just rhead) (Just cu') (Just ct) 0 i j
                            setAttr right rl' (Just p)
                            setAttr left rhead (Just p)
                            setAttr down cu' (Just p)
                            setAttr up ct (Just p)
                            setAttr ctl p ct
                        Nothing -> return ()  -- Handle Nothing case for cu
                  Nothing -> return ()  -- Handle Nothing case for rl
            else return ()
        rl <- getAttr left rhead
        rr <- getAttr right rhead
        case (rl, rr) of
          (Just rl', Just rr') -> do
              setAttr right rl' (Just rr')
              setAttr left rr' (Just rl')
          _ -> return ()  -- Handle Nothing case for rl or rr
    return h


-- Modifications for forEach and forEach' to handle retrieval actions properly

forEach :: (DNode s -> ST s (Maybe (DNode s))) -> DNode s -> (DNode s -> ST s ()) -> ST s ()
forEach step start f = do
    next <- step start
    case next of
        Just now -> if now /= start
                    then do
                        f now
                        forEach step now f  -- recursively continue to next node
                    else return ()
        Nothing -> return ()

-- Modified forEach' function that collects results in a list
forEach' :: (DNode s -> ST s (Maybe (DNode s))) -> DNode s -> (DNode s -> ST s a) -> ST s [a]
forEach' step start f = do
    next <- step start
    case next of
        Just now -> if now /= start
                    then do
                        r <- f now
                        rs <- forEach' step now f  -- recursively accumulate results
                        return (r:rs)
                    else return []
        Nothing -> return []



setCover :: DNode s -> ST s ()
setCover pctl = do
    cl <- getAttr left pctl
    cr <- getAttr right pctl
    case (cl, cr) of
      (Just cl', Just cr') -> do
          setAttr right cl' (Just cr')
          setAttr left cr' (Just cl')
      _ -> return ()  -- Handle the case where cl or cr is Nothing
    -- Pattern match on down first, then use forEach
    forEachDown pctl
  where
    forEachDown :: DNode s -> ST s ()
    forEachDown pctl = do
        downNode <- getAttr down pctl
        case downNode of
          Just p' -> forEach (getAttr right) p' $ \q -> do
            qu <- getAttr up q
            qd <- getAttr down q
            qct <- getAttr ctl q
            qcs <- getAttr size qct
            case (qu, qd) of
              (Just qu', Just qd') -> do
                  setAttr down qu' (Just qd')
                  setAttr up qd' (Just qu')
              _ -> return ()  -- Handle Nothing case
            setAttr size qct (qcs - 1)
            forEachDown pctl  -- Recursively handle the next down node
          Nothing -> return ()  -- Handle the Nothing case



setUncover :: DNode s -> ST s ()
setUncover pctl = do
    cl <- getAttr left pctl
    cr <- getAttr right pctl
    case (cl, cr) of
      (Just cl', Just cr') -> do
          setAttr right cl' (Just pctl)
          setAttr left cr' (Just pctl)
      _ -> return ()  -- Handle the case where cl or cr is Nothing
    forEachUp pctl
  where
    forEachUp :: DNode s -> ST s ()
    forEachUp pctl = do
        upNode <- getAttr up pctl
        case upNode of
          Just p' -> forEach (getAttr left) p' $ \q -> do
            qu <- getAttr up q
            qd <- getAttr down q
            qct <- getAttr ctl q
            qcs <- getAttr size qct
            case (qu, qd) of
              (Just qu', Just qd') -> do
                  setAttr down qu' (Just q)
                  setAttr up qd' (Just q)
              _ -> return ()  -- Handle Nothing case
            setAttr size qct (qcs + 1)
            forEachUp pctl  -- Recursively handle the next up node
          Nothing -> return ()  -- Handle Nothing case



solve :: [UArray Int Bool] -> Int -> ST s [Bool]
solve bitmap ncol = do
    dlx <- buildDLX bitmap (length bitmap) ncol
    results <- solve' dlx 0 []
    return (concat results)  -- Flatten the list of lists to a single list

solve' :: DNode s -> Int -> [Int] -> ST s [[Bool]]
solve' head step plan = do
    -- Get the left attribute of the head node
    hl <- getAttr left head
    -- Check if the head node has looped back to itself
    case hl of
        -- If it loops back to itself, a solution has been found
        Just hl' | hl' == head -> return [[True]]
        -- Otherwise, continue searching for solutions
        _ -> do
            -- Create a reference for the best column node with initial high size and Null node
            best <- newSTRef (9999, Null)
            -- Iterate through the nodes to find the best column node to cover
            forEach (getAttr right) head $ \p -> do
                sp <- getAttr size p  -- Get the size of the current node
                (m, _) <- readSTRef best
                -- Update the best column node if the current one is smaller
                when (sp < m) $ writeSTRef best (sp, p)
            -- Read the best column node from the reference
            (_, y) <- readSTRef best
            -- If no valid column node is found, return an empty result
            case y of
                Null -> return []  -- No best node found
                _ -> do
                    -- Cover the best column node
                    setCover y
                    -- Iterate over the rows and recursively attempt to solve
                    res <- forEach' (getAttr down) y $ \p -> do
                        -- Read the row value of the current node
                        rowValue <- getAttr row p
                        -- Cover the nodes in the current row
                        forEach (getAttr right) p $ \q -> do
                            qctl <- getAttr ctl q
                            setCover qctl
                        -- Recursively solve for the next step
                        r' <- solve' head (step + 1) (rowValue : plan)
                        -- Uncover the nodes in the current row after backtracking
                        forEach (getAttr left) p $ \q -> do
                            qctl <- getAttr ctl q
                            setUncover qctl
                        -- Return the result for this path
                        return r'
                    -- Uncover the best column node after backtracking
                    setUncover y
                    -- Flatten the result and return
                    return (concat res)

-- Parse the Sudoku board from file
parseBoard :: FilePath -> IO Board
parseBoard filePath = do
    content <- readFile filePath
    let (sudokuN : boardLines) = lines content
        boardSize = read sudokuN :: Int
        board = map (map read . split ',' . filter (/= ' ')) boardLines
    return board


-- Split function to handle comma-separated values
split :: Char -> String -> [String]
split _ [] = []
split delimiter s = let (word, rest) = break (== delimiter) s in word : case rest of
    [] -> []
    (_:xs) -> split delimiter xs


-- Test a board by file
testBoardFromFile :: FilePath -> IO ()
testBoardFromFile filePath = do
    board <- parseBoard filePath
    let result = numSolutions board
    putStrLn $ "Board: " ++ filePath ++ " Result: " ++ show result


-- Main function to test all boards
main :: IO ()
main = do
    let basePath = "/Users/ljy/Desktop/functional/individual/dataset/"
    let testFiles = ["board0.txt", "board1.txt", "board2.txt", "board3.txt", "board4.txt",
                     "board5.txt", "board6.txt", "board7.txt", "board8.txt", "board9.txt"]
    mapM_ (testBoardFromFile . (basePath ++)) testFiles
