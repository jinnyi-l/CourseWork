-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module SudokuSolver(Board, Solutions(..), author, nickname, numSolutions) where

import Sudoku (Board, Solutions(..))
import Control.Monad
import Control.Monad.ST
import Data.List
import Data.STRef

author :: String
author = "Jinyi Li"

nickname :: String
nickname = "SudokuMaster"

-- numSolutions takes a Sudoku board and returns the number of valid solutions
numSolutions :: Board -> Solutions
numSolutions board = 
    let solutions = solve (boardToBitmap board) 324  -- Call the solve function with the bitmap
    in case length solutions of
        0 -> NoSolution
        1 -> UniqueSolution
        _ -> MultipleSolutions

-- Helper function to safely access elements from a list with logging
safeGetWithLogging :: Show a => [a] -> Int -> Maybe a
safeGetWithLogging xs i = 
    if i >= 0 && i < length xs 
    then Just (xs !! i)
    else error $ "Index too large: " ++ show i ++ ", length: " ++ show (length xs) ++ ", index: " ++ show i

-- Helper function to convert the board to the bitmap representation used by solve
boardToBitmap :: Board -> [[Bool]]
boardToBitmap prob = 
    let sudokuN = 3
        blockN = sudokuN ^ 2
        secN = blockN ^ 2
        colN = secN * 4
        fixed = filter (\(_, _, d) -> d /= 0) $
                    concatMap (\(r, l) -> (zipWith (\c x -> (r, c, x)) [0..]) l) (zip [0..] prob)
        fixedPos = map (\(a, b, _) -> (a, b)) fixed
        all = fixed ++ [(x, y, d) | x <- [0..blockN - 1], y <- [0..blockN - 1],
                                   d <- [1..blockN], (x, y) `notElem` fixedPos]
        makeBits [] n = replicate n False
        makeBits (x:xs) n = replicate x False ++ (True : makeBits (map (\t -> t - x - 1) xs) (n - x - 1))
    in [makeBits [x * blockN + y,
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
                     }

-- Define an Eq instance for DNode
instance Eq (DNode s) where
    a == b = left a == left b

-- Covering and uncovering columns in Dancing Links (DLX)
setCover :: DNode s -> ST s ()
setCover pctl = do
    cl <- getAttr left pctl
    cr <- getAttr right pctl
    case (cl, cr) of
        (Just l, Just r) -> do
            setAttr right l (Just r)
            setAttr left r (Just l)
        _ -> return ()
    forEach (getAttr down) pctl $ \p -> 
        forEach (getAttr right) p $ \q -> do
            qu <- getAttr up q
            qd <- getAttr down q
            case (qu, qd) of
                (Just u, Just d) -> do
                    setAttr down u (Just d)
                    setAttr up d (Just u)
                _ -> return ()
            qct <- getAttr ctl q
            qcs <- getAttr size qct
            setAttr size qct (qcs - 1)

setUncover :: DNode s -> ST s ()
setUncover pctl = do
    cl <- getAttr left pctl
    cr <- getAttr right pctl
    case (cl, cr) of
        (Just l, Just r) -> do
            setAttr right l (Just pctl)
            setAttr left r (Just pctl)
        _ -> return ()
    forEach (getAttr up) pctl $ \p -> 
        forEach (getAttr left) p $ \q -> do
            qu <- getAttr up q
            qd <- getAttr down q
            case (qu, qd) of
                (Just u, Just d) -> do
                    setAttr down u (Just q)
                    setAttr up d (Just q)
                _ -> return ()
            qct <- getAttr ctl q
            qcs <- getAttr size qct
            setAttr size qct (qcs + 1)

-- Solve function
solve :: [[Bool]] -> Int -> [[[Int]]]
solve bitmap ncol = runST $ do
    dlx <- buildDLX bitmap (length bitmap) ncol
    solve' dlx 0 []
  where
    solve' head step plan = do
        hl <- getAttr left head
        hr <- getAttr right head
        if sameRef hl (Just head)
          then return [[plan]]  -- Base case: Return a 3D list [[[Int]]]
          else do
            best <- newSTRef (9999, head)
            forEach (getAttr right) head $ \p -> do
                sp <- getAttr size p
                (m, _) <- readSTRef best
                when (sp < m) (writeSTRef best (sp, p))
            (_, y) <- readSTRef best
            setCover y
            res <- forEach' (getAttr down) y $ \p -> do
                forEach (getAttr right) p $ \q -> do
                    qctl <- getAttr ctl q
                    setCover qctl
                rowValue <- readSTRef (row p)
                r' <- solve' head (step + 1) (rowValue : plan)
                forEach (getAttr left) p $ \q -> do
                    qctl <- getAttr ctl q
                    setUncover qctl
                return r'
            setUncover y
            return (concat res)

-- Utility function to compare `STRef`s of `DNode s`
sameRef :: Maybe (DNode s) -> Maybe (DNode s) -> Bool
sameRef (Just a) (Just b) = left a == left b
sameRef Nothing Nothing = True
sameRef _ _ = False

buildDLX :: [[Bool]] -> Int -> Int -> ST s (DNode s)
buildDLX bitmap rows cols = do
    head <- newDNode  -- Create the root header node (head of the DLX matrix)

    columnHeaders <- forM [0..cols-1] $ \_ -> newDNode  -- Create an array of column headers (one for each column)

    -- Link column headers to each other in a circular doubly-linked list
    forM_ (zip columnHeaders (tail columnHeaders ++ [head])) $ \(prev, next) -> do
        setAttr right prev (Just next)
        setAttr left next (Just prev)

    -- Link the head to the first and last column headers
    setAttr right head (Just (Prelude.head columnHeaders))
    setAttr left (Prelude.head columnHeaders) (Just head)

    -- Create the rows and link the nodes horizontally and vertically
    forM_ bitmap $ \row -> do
        prevNodeRef <- newSTRef Nothing
        firstNodeRef <- newSTRef Nothing

        forM_ (zip [0..] row) $ \(j, value) -> do
            when value $ do
                if j < length columnHeaders then do
                    newNode <- newDNode
                    let colHeader = columnHeaders !! j
                    colDown <- getAttr down colHeader
                    setAttr up newNode (Just colHeader)
                    setAttr down colHeader (Just newNode)
                    case colDown of
                        Just downNode -> setAttr down newNode (Just downNode) >> setAttr up downNode (Just newNode)
                        Nothing -> return ()
                    setAttr ctl newNode colHeader
                    modifySTRef' (size colHeader) (+1)
                    modifySTRef' prevNodeRef (const (Just newNode))
                    maybe (writeSTRef firstNodeRef (Just newNode)) (`setAttr` newNode) =<< readSTRef prevNodeRef
                else
                    error $ "Index " ++ show j ++ " out of bounds for columnHeaders with length " ++ show (length columnHeaders)
        
        maybe (return ()) (\firstN -> maybe (return ()) (\lastN -> setAttr left firstN (Just lastN) >> setAttr right lastN (Just firstN)) =<< readSTRef prevNodeRef) =<< readSTRef firstNodeRef

    return head


newDNode :: ST s (DNode s)
newDNode = do
    l <- newSTRef Nothing
    r <- newSTRef Nothing
    u <- newSTRef Nothing
    d <- newSTRef Nothing
    c <- newSTRef undefined
    s <- newSTRef 0
    row <- newSTRef 0
    return DNode { left = l, right = r, up = u, down = d, ctl = c, size = s, row = row }

getAttr :: (DNode s -> STRef s a) -> DNode s -> ST s a
getAttr f dnode = readSTRef (f dnode)

setAttr :: (DNode s -> STRef s a) -> DNode s -> a -> ST s ()
setAttr f dnode val = writeSTRef (f dnode) val

forEach :: (Eq a) => (a -> ST s (Maybe a)) -> a -> (a -> ST s ()) -> ST s ()
forEach step start f = step start >>= loop
  where
    loop (Just now) = when (now /= start) (f now >> step now >>= loop)
    loop Nothing = return ()

forEach' :: (Eq a) => (a -> ST s (Maybe a)) -> a -> (a -> ST s b) -> ST s [b]
forEach' step start f = step start >>= loop
  where
    loop (Just now) = if now /= start
                      then do
                          r <- f now
                          rs <- step now >>= loop
                          return (r : rs)
                      else return []
    loop Nothing = return []
