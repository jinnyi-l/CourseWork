module Main where

import SudokuSolver (Board, Solutions(..), numSolutions)

-- Define test boards with dynamic sizes
uniqueBoard1 :: Board
uniqueBoard1 = [[0,1,6,3,9,2,0,5,0],
                [4,5,8,6,0,7,0,9,3],
                [2,0,3,0,0,4,7,1,0],
                [0,0,0,0,3,5,0,0,0],
                [1,0,0,7,6,9,5,3,2],
                [0,3,2,4,0,0,9,0,0],
                [8,0,0,1,4,0,3,7,0],
                [3,0,9,5,2,8,0,4,0],
                [6,0,0,9,0,0,0,0,5]]

invalidBoard16 :: Board
invalidBoard16 = [
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7],
    [0,0,0,0,0,6,11,7,9,0,2,12,0,0,0,0],
    [0,0,0,13,0,0,0,0,0,14,0,11,0,0,0,0],
    [0,0,0,0,0,2,12,0,0,0,0,0,0,0,15,1],
    [0,0,0,0,0,0,0,2,0,0,1,0,0,11,0,0],
    [0,0,10,0,5,0,8,0,15,11,0,0,0,12,0,6],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,0],
    [0,0,0,0,12,9,0,0,0,13,0,4,0,0,1,0],
    [0,0,4,0,0,0,0,0,5,0,0,16,0,0,0,0],
    [0,13,8,0,1,0,0,5,0,0,0,0,0,0,0,0],
    [0,0,0,15,0,0,0,0,13,0,0,0,16,0,0,0],
    [0,5,0,0,0,0,15,0,0,0,0,6,0,0,0,0],
    [6,0,0,0,2,0,0,0,0,0,0,10,1,15,0,0],
    [0,0,0,0,15,0,0,0,14,0,0,0,0,0,13,4],
    [0,0,13,0,0,5,10,8,3,0,0,0,0,6,0,0],
    [0,3,11,1,0,0,0,0,0,0,0,0,10,0,5,8]
    ]


invalidBoard1 :: Board
invalidBoard1 = [[0,1,6,3,9,2,0,5,0],
                 [4,5,8,6,0,7,0,9,3],
                 [2,0,3,0,0,4,7,1,0],
                 [0,0,0,0,3,5,0,0,0],
                 [1,0,0,7,0,9,5,3,2],
                 [0,3,2,4,0,0,9,0,0],
                 [8,0,0,1,4,0,3,7,0],
                 [3,0,9,5,2,8,7,4,0],
                 [6,0,0,9,0,0,0,0,5]]



uniqueBoardC :: Board
uniqueBoardC = [[0,0,12,19,23,15,27,17,22,20,0,16,3,28,9,10,8,18,0,32,0,2,29,0,0,14,6,33,11,35,25,5,21,26,4,13],
    [35,33,6,11,7,14,29,31,32,2,34,36,24,1,0,0,15,12,25,21,4,26,5,13,9,8,18,3,10,0,17,27,22,0,16,30],
    [28,3,18,10,9,8,5,25,21,0,13,4,33,35,0,11,14,6,17,22,0,20,27,30,23,15,12,24,19,1,31,29,0,2,36,34],
    [16,17,22,27,20,30,0,33,6,7,14,35,25,4,26,0,13,21,3,18,28,9,10,8,2,34,32,31,29,36,24,19,12,23,0,15],
    [36,31,32,29,2,34,10,3,0,9,8,28,17,16,20,27,30,22,24,12,0,23,19,15,0,13,21,25,5,4,33,11,6,7,35,14],
    [4,25,21,5,26,13,19,24,12,23,15,1,31,36,2,29,34,32,33,6,35,7,0,0,20,30,22,17,27,16,3,10,18,9,28,8],
    [20,22,15,1,27,17,16,6,30,11,0,7,21,26,5,28,0,8,18,0,9,10,36,3,29,31,0,32,35,2,12,4,13,19,23,0],
    [2,32,14,35,29,31,36,18,34,0,3,9,22,20,27,1,17,15,12,13,23,19,4,24,5,25,8,21,28,0,6,16,0,11,7,33],
    [26,0,8,28,5,25,4,12,13,19,24,0,32,2,29,35,0,14,6,30,7,11,16,33,27,17,15,22,1,0,18,36,0,10,0,3],
    [9,18,34,36,10,3,28,21,8,0,25,26,6,7,11,16,33,30,22,0,20,27,1,17,19,24,13,12,4,23,0,35,14,29,2,0],
    [23,12,13,4,19,24,1,22,15,27,17,20,18,9,0,36,3,34,32,14,2,29,0,31,11,0,30,6,0,7,21,28,8,5,26,25],
    [7,6,0,16,11,33,35,0,14,29,31,2,12,23,19,4,24,13,21,8,26,5,28,25,10,3,34,18,0,9,22,1,0,27,0,17],
    [15,0,23,12,24,27,0,16,20,0,11,30,28,0,3,0,5,9,36,2,34,31,32,10,33,29,7,35,6,0,4,21,26,25,13,19],
    [30,16,20,0,17,11,0,35,7,33,29,14,4,13,25,21,19,26,28,9,0,3,18,0,31,10,2,36,32,0,1,12,23,24,15,27],
    [13,4,26,0,25,19,12,1,0,0,0,0,36,34,31,32,10,0,35,7,14,33,6,29,17,11,20,16,22,30,28,18,9,3,8,5],
    [34,36,2,32,31,10,18,28,9,3,5,8,16,30,17,22,11,0,1,23,15,24,12,27,25,19,26,4,21,13,35,6,7,33,14,29],
    [14,35,7,6,33,29,32,36,2,31,10,34,0,15,0,0,0,23,4,26,13,25,21,19,3,5,9,28,18,8,16,22,20,17,30,11],
    [8,28,9,18,3,5,21,4,26,25,0,13,35,14,33,6,29,7,16,20,30,17,22,11,24,27,23,0,0,15,0,32,2,31,34,10],
    [21,0,28,3,8,26,25,19,4,13,23,12,29,32,14,33,2,35,11,16,6,30,17,7,0,0,1,27,0,22,10,31,36,34,18,0],
    [6,11,16,17,30,7,33,29,35,0,2,32,19,12,13,25,0,4,5,28,21,8,3,26,34,0,36,10,31,18,27,24,1,0,22,20],
    [18,10,36,31,34,9,3,5,28,8,26,21,11,6,30,17,7,16,27,1,22,0,24,20,13,23,4,19,25,12,29,33,35,14,32,2],
    [12,19,0,25,13,23,24,27,1,0,20,22,10,0,34,31,9,36,29,35,32,14,33,2,30,7,0,11,17,6,0,3,28,8,21,26],
    [22,0,1,0,15,20,17,11,16,30,7,6,5,21,8,3,26,28,0,36,18,34,31,9,14,2,0,29,33,32,19,25,4,13,0,23],
    [32,29,35,33,14,2,31,10,36,34,9,18,27,22,0,24,20,1,19,4,12,13,25,0,8,26,28,5,3,21,11,17,16,30,6,0],
    [10,34,31,2,0,18,9,8,3,28,21,5,30,0,16,20,6,17,15,24,27,1,23,22,0,12,25,13,26,19,0,7,33,35,29,32],
    [29,14,33,7,0,32,0,34,0,36,18,10,15,27,1,0,0,24,13,25,19,0,26,12,28,21,3,0,9,5,30,20,17,16,11,6],
    [5,8,3,9,28,0,26,13,25,0,12,19,14,29,35,7,32,0,0,17,11,0,0,6,0,22,24,15,23,27,34,2,31,36,10,18],
    [19,13,25,26,4,12,23,15,24,1,22,27,34,10,0,2,18,31,14,33,29,35,7,32,16,6,0,30,20,11,8,9,3,28,5,21],
    [27,15,24,23,1,22,20,30,17,16,6,11,8,5,28,9,21,3,34,31,10,36,0,18,35,32,33,14,7,29,0,26,25,4,19,12],
    [11,30,17,20,16,6,7,0,0,35,32,29,13,0,4,26,12,25,8,3,0,0,9,21,36,18,31,34,2,10,15,23,24,1,27,22],
    [17,20,27,15,0,16,30,7,11,6,35,33,26,25,21,8,4,5,9,10,3,18,34,28,32,36,29,2,0,31,23,13,19,12,24,1],
    [25,26,5,0,21,4,0,23,19,12,1,24,0,31,32,14,36,29,7,11,33,6,30,35,0,16,27,20,15,17,9,0,10,18,0,28],
    [3,9,10,0,18,28,0,26,5,21,4,25,7,33,6,30,35,11,20,27,17,22,0,0,12,1,19,23,13,24,0,0,29,32,31,36],
    [0,2,29,14,32,36,0,9,10,0,28,0,20,0,22,15,16,27,23,19,24,12,13,1,21,4,5,26,8,25,0,30,11,0,33,35],
    [24,23,19,13,12,1,15,0,27,22,16,17,9,3,18,0,28,10,2,29,31,32,14,36,6,35,11,7,30,33,26,8,5,21,25,4],
    [33,7,11,30,6,35,14,0,29,32,36,31,23,24,12,13,1,19,26,5,25,21,8,0,18,28,10,9,34,3,20,15,27,22,17,16]]
    
    
invalidBoard49 :: Board
invalidBoard49 = [
    [0,32,0,5,0,0,0,0,4,0,0,0,0,0,0,0,0,18,0,0,47,0,42,0,0,0,0,0,0,0,0,26,0,0,0,1,0,0,0,0,0,0,0,0,3,0,0,0,0],
    [0,0,0,0,0,0,43,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,38,0,0,0,0,0,18,0,0,0,7,0,0,0,0,0,0,0,0,0,0,39,22,0,0,46],
    [40,0,0,0,0,0,0,0,22,6,0,0,0,0,0,15,0,0,0,0,35,0,29,43,0,0,0,44,0,23,14,0,17,0,0,0,0,16,5,0,34,0,0,0,0,0,0,37,0],
    [0,0,1,0,45,0,0,0,0,0,0,36,0,0,0,0,0,0,0,2,0,0,0,30,7,0,0,0,43,0,0,21,4,0,0,0,0,22,0,0,0,6,11,0,0,0,38,34,0],
    [0,0,0,0,0,0,0,12,0,0,34,0,0,0,0,0,0,0,39,0,26,0,0,41,0,17,0,0,2,40,0,0,9,0,0,0,7,0,0,0,37,0,44,0,13,0,0,0,0],
    [0,0,0,46,0,0,0,0,0,0,0,7,49,0,34,0,5,0,0,38,0,20,28,0,0,0,0,0,0,19,42,0,0,36,0,0,13,0,0,0,0,0,0,0,0,0,0,0,0],
    [18,0,0,49,0,0,0,0,17,0,0,8,0,0,0,4,0,0,0,0,21,0,0,0,0,0,0,6,0,0,0,0,16,32,0,20,0,0,0,0,0,0,0,0,36,0,0,27,42],
    [0,0,0,0,0,6,0,0,0,43,0,0,47,0,0,0,0,0,0,0,0,0,20,0,25,0,28,0,36,33,35,15,0,0,0,0,48,0,0,0,29,41,0,17,45,0,0,14,1],
    [30,0,0,0,0,11,0,0,44,41,0,0,21,0,0,0,0,0,37,0,31,15,35,0,0,0,0,0,0,38,0,0,0,10,46,0,0,0,0,0,14,0,0,0,0,40,0,0,0],
    [0,0,0,0,0,23,8,0,0,33,0,0,0,0,0,0,20,24,25,0,0,31,0,0,0,0,0,0,0,41,0,0,44,48,29,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,13,9,0,0,0,0,0,3,0,0,0,2,0,0,17,0,0,0,0,0,5,0,7,0,47,0,0,0,0,0,0,0,0,36,0,0,0,0,0,0,0,46,0],
    [0,0,15,0,0,0,0,0,0,30,0,0,12,0,0,0,0,38,0,0,0,0,1,8,0,23,0,0,0,0,0,0,40,0,28,0,0,0,0,7,0,43,0,0,0,0,0,29,0],
    [0,37,0,47,49,0,0,17,0,2,14,0,0,0,0,0,0,41,48,0,4,0,0,0,0,0,0,0,32,0,0,0,0,0,5,0,25,0,0,0,0,24,33,0,0,0,0,0,0],
    [24,0,0,20,0,0,0,0,6,38,0,0,0,39,0,19,0,0,0,0,0,4,0,0,0,0,0,0,8,2,0,0,23,0,0,0,0,0,0,32,0,30,0,0,0,0,0,49,0],
    [0,0,0,0,0,0,26,0,0,29,18,0,0,0,0,0,0,0,16,12,0,36,0,20,0,0,0,42,0,0,0,0,0,0,19,0,0,45,0,0,0,0,0,0,0,0,0,0,0],
    [0,31,0,43,18,0,0,0,0,0,0,0,2,0,0,0,0,14,0,0,8,0,0,0,0,34,0,5,0,0,30,0,37,0,0,36,0,27,0,0,0,0,0,0,0,0,0,0,33],
    [0,0,0,24,0,27,20,0,0,0,0,0,0,0,0,0,33,0,0,0,0,0,41,21,0,0,44,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,29,0,0,48,0,0,0],
    [49,0,0,0,0,37,0,0,45,0,0,0,41,21,0,48,0,0,31,47,0,0,33,35,15,0,19,46,26,0,0,0,0,0,6,3,0,0,0,1,0,0,0,0,0,0,0,0,0],
    [0,4,0,41,0,0,0,0,0,42,0,0,0,0,23,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,0,33,0,0,0,0,0,0,0,0,6,0],
    [28,0,0,0,23,0,0,0,0,0,0,0,0,0,0,0,24,0,0,20,36,13,0,47,0,0,0,0,0,0,0,0,0,0,44,0,0,0,38,0,0,0,49,0,0,0,12,0,0],
    [0,0,0,0,0,0,35,0,0,49,0,0,30,0,0,0,0,5,0,0,32,0,0,0,0,0,0,0,0,0,0,36,0,9,40,0,31,0,43,0,0,0,0,0,0,0,0,0,41],
    [0,0,0,0,0,47,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,0,28,0,0,0,0,0,0,0,0,24,35,0,0,0],
    [0,0,0,0,0,0,0,49,0,0,7,0,0,0,0,0,0,0,38,0,0,0,0,0,0,20,0,9,0,15,0,42,0,0,36,0,0,21,0,0,13,0,17,14,0,0,0,0,0],
    [0,0,0,0,0,0,18,28,0,9,0,0,0,23,0,0,0,0,0,44,0,0,34,0,0,12,0,0,0,31,0,49,0,0,0,0,0,0,0,40,0,0,22,46,0,0,0,0,0],
    [0,0,0,0,0,12,0,0,21,0,0,0,48,0,7,0,0,0,30,11,0,0,0,0,24,0,36,15,0,0,0,46,0,33,0,0,0,1,0,0,0,17,9,0,2,20,0,0,25],
    [0,0,0,0,0,0,0,5,0,0,32,0,0,0,0,26,10,0,0,0,0,0,45,0,0,0,0,0,0,0,25,28,20,0,0,0,0,0,0,0,7,0,0,0,0,0,0,0,48],
    [0,0,28,0,0,20,0,0,0,0,0,33,0,0,0,0,0,0,0,0,42,0,48,0,0,21,0,4,0,0,0,0,1,0,0,0,0,0,34,6,0,0,31,0,0,0,0,0,0],
    [0,0,0,0,0,0,44,0,0,0,0,0,0,0,0,20,0,0,2,23,0,49,0,0,0,0,0,0,0,0,48,0,0,0,13,46,0,0,0,0,0,0,0,0,0,0,0,32,34],
    [0,0,0,0,0,13,0,0,0,25,0,1,0,14,0,0,0,0,0,0,0,0,6,46,26,32,0,0,0,0,11,0,7,0,0,24,0,0,0,28,9,27,10,0,0,0,0,0,0],
    [0,0,0,19,0,0,0,0,0,0,0,0,0,5,0,0,0,34,0,0,0,0,23,14,1,0,17,25,0,0,0,0,0,0,0,0,47,0,0,0,31,48,45,0,0,8,0,0,44],
    [0,0,0,44,4,0,0,0,0,27,0,0,40,0,0,0,23,0,0,14,2,0,0,0,0,0,16,37,0,48,0,0,0,47,0,33,0,0,0,0,0,0,0,0,0,0,46,22,0],
    [0,0,0,0,0,36,0,0,32,0,22,0,0,46,0,0,0,0,0,0,33,0,0,0,0,8,0,0,0,0,0,0,3,1,0,0,0,0,11,0,0,0,48,43,0,0,0,0,0],
    [0,0,0,6,0,0,46,0,13,48,0,0,0,49,16,0,11,0,0,0,0,0,0,0,20,0,0,0,0,10,19,0,39,0,15,0,21,0,0,29,0,0,0,0,1,0,0,0,0],
    [0,0,0,11,0,0,0,41,8,45,0,21,0,29,0,0,0,48,47,0,0,0,19,0,35,0,15,0,46,34,6,0,0,26,22,0,1,3,0,0,17,0,0,0,0,0,0,9,0],
    [25,1,0,23,0,3,0,33,0,0,15,0,0,42,0,0,0,0,0,0,0,43,0,0,0,0,0,0,0,45,0,0,0,0,4,38,0,32,0,46,0,0,37,0,0,0,0,0,0],
    [0,5,0,0,0,0,0,0,0,0,0,0,4,0,0,43,0,0,0,0,18,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,45,0,0,0,0,0,0,25,0,0],
    [39,42,19,0,0,33,0,0,0,0,12,5,0,34,0,38,22,0,46,0,0,0,0,0,0,0,0,0,0,36,0,0,0,28,0,0,0,0,0,0,0,0,8,0,29,0,48,0,4],
    [0,0,0,0,0,0,0,0,0,13,47,0,31,0,12,0,0,0,0,0,0,40,9,25,0,0,0,36,27,0,0,0,33,0,35,0,0,41,0,0,0,8,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,46,0,0,0,0,0,39,42,0,0,0,0,48,29,0,21,8,0,0,0,0,2,0,0,11,5,0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,18,0,0,0,0,0,0,0,0,14,0,0,0,0,0,8,29,0,44,0,22,10,0,0,0,0,34,0,16,0,0,0,0,40,0,24,0,0,20,36,39,0,42,0,0,35,0],
    [3,0,0,0,1,0,0,0,33,0,35,0,0,0,20,0,0,0,0,0,0,18,0,0,0,43,0,0,48,0,0,44,0,0,0,0,0,0,0,0,0,0,0,0,5,0,34,0,0],
    [0,0,0,0,21,0,0,0,24,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,30,0,0,0,13,0,0,0,49,0,0,42,0,15,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,20,0,23,3,0,0,0,0,0,0,0,45,34,0,0,6,0,0,0,0,0,7,0,49,0,0,0,40,0,0,0,24,0,0,0,0,0,0,0,0],
    [47,0,0,7,30,49,0,0,14,0,41,0,0,4,43,0,0,0,0,31,48,0,0,0,19,0,0,26,0,0,0,0,5,0,38,0,0,0,0,0,2,20,35,0,0,0,0,0,0],
    [0,0,34,0,0,0,22,0,0,0,0,0,0,0,0,0,7,0,11,0,0,27,0,0,0,0,0,0,15,26,39,0,46,19,0,0,0,0,0,4,0,1,0,0,0,0,0,2,3],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,15,0,0,0,35,0,0,0,48,13,0,0,0,43,21,0,1,0,0,0,44,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,39,0,0,15,0,0,0,0,0,0,0,38,0,0,0,0,0,34,25,0,0,0,0,0,0,0,0,36,27,0,40,0,0,0,0,0,31,0,21,0,45,44,0,0,0,8],
    [0,0,0,0,24,42,9,0,0,0,0,0,32,0,0,0,0,0,19,0,0,0,0,0,0,0,0,0,17,20,0,25,0,23,0,0,11,49,7,0,30,0,0,0,0,29,0,0,0],
    [1,0,0,0,0,0,0,0,0,0,0,0,0,9,2,0,3,20,0,0,0,0,7,16,0,0,0,0,0,0,0,0,0,0,0,10,0,46,0,0,33,0,0,0,0,0,0,0,0]
    ]



invalidBoardC :: Board
invalidBoardC = [[6,0,27,13,29,24,0,21,1,7,23,0,0,2,35,14,8,11,28,0,0,34,0,16,25,31,0,33,4,0,32,17,12,36,20,15],
    [12,17,32,15,36,20,18,25,0,10,33,4,26,0,0,0,5,0,11,2,8,35,22,14,21,1,0,0,3,30,27,19,6,29,24,13],
    [3,0,7,1,23,0,26,0,5,16,9,34,0,0,0,32,15,0,0,29,13,6,0,0,11,0,14,0,35,22,10,25,4,33,18,0],
    [0,11,14,8,2,0,20,17,15,32,36,12,30,23,3,0,1,21,25,0,31,4,18,10,19,13,27,29,6,0,0,28,34,0,26,5],
    [4,25,10,31,0,18,22,0,8,0,2,35,0,0,6,27,13,19,17,0,15,12,20,32,0,0,16,9,34,0,7,21,3,23,30,1],
    [0,28,16,5,9,26,24,19,13,27,29,6,0,33,4,10,31,25,0,23,1,3,30,7,17,15,32,36,0,0,14,0,35,2,22,8],
    [5,4,0,0,10,25,0,35,2,22,14,13,0,27,0,24,29,0,12,32,36,1,0,0,34,9,26,16,15,28,30,3,8,7,0,23],
    [31,6,24,29,27,19,21,3,0,30,0,8,0,14,13,22,0,35,0,16,0,15,28,26,0,0,0,10,5,25,20,12,1,32,17,36],
    [15,34,0,9,0,28,19,6,0,24,27,31,0,0,5,0,0,4,3,7,0,0,0,30,12,36,20,32,1,17,22,0,0,14,11,2],
    [13,35,22,0,0,11,17,0,36,20,32,0,21,7,8,30,23,3,4,10,33,0,0,18,6,29,24,0,31,0,26,34,15,0,28,9],
    [8,3,30,23,7,21,0,0,9,0,16,15,17,32,1,20,36,12,0,27,29,0,0,24,35,2,22,14,13,11,18,4,5,10,25,0],
    [1,12,0,36,32,17,25,0,0,18,10,5,0,16,15,0,9,34,0,14,2,13,0,0,3,23,0,7,8,0,24,0,31,27,0,29],
    [24,0,13,11,0,29,23,0,0,1,12,30,2,3,22,8,21,0,16,4,25,26,9,5,10,19,31,6,18,0,15,32,20,34,36,28],
    [22,0,8,21,3,2,36,32,28,15,0,20,23,12,30,1,17,7,10,6,19,18,0,31,27,0,13,0,24,29,5,16,0,4,9,0],
    [30,7,0,17,12,0,0,0,25,5,4,26,0,34,20,15,28,32,27,35,0,24,29,13,14,21,0,0,22,2,31,10,0,6,33,19],
    [0,32,15,28,0,36,33,0,19,0,6,18,9,4,26,5,25,16,0,0,21,0,2,0,7,17,1,0,30,23,13,27,0,35,29,11],
    [26,16,5,25,4,9,0,0,11,0,0,24,33,6,18,31,19,0,7,0,0,30,23,1,32,28,15,0,20,36,8,14,22,0,0,21],
    [0,10,31,19,0,0,0,14,21,8,0,0,29,0,24,13,11,0,32,34,28,20,36,15,16,25,0,4,26,9,1,7,30,12,0,17],
    [29,13,11,14,22,35,12,0,32,17,20,0,3,30,2,21,7,8,5,18,10,9,4,25,0,0,0,24,33,0,28,15,0,26,34,16],
    [0,15,28,16,26,0,0,31,27,0,24,0,4,18,9,25,10,5,0,30,0,0,3,0,0,0,0,20,23,12,11,13,29,22,35,14],
    [23,0,17,0,20,12,4,5,10,25,0,9,34,26,36,28,0,0,0,22,14,29,35,11,0,7,21,0,0,3,19,31,33,0,6,27],
    [33,31,19,0,24,0,3,8,0,21,30,2,35,22,29,11,14,0,15,26,16,36,34,28,0,10,25,18,9,4,17,0,23,20,12,32],
    [0,5,25,0,18,4,35,13,14,11,0,29,6,24,0,19,0,31,1,20,32,23,12,17,15,16,28,26,36,34,0,8,2,0,3,7],
    [2,8,0,7,0,3,34,15,16,0,26,36,0,20,23,0,32,1,31,24,27,0,0,19,13,14,0,22,29,35,25,5,9,18,4,0],
    [25,18,33,6,31,10,0,22,0,2,0,0,0,13,0,0,0,24,20,15,34,17,0,36,26,4,9,5,28,16,0,0,21,1,0,12],
    [0,26,0,4,5,16,27,24,35,29,13,19,0,0,25,33,6,0,0,1,0,21,7,23,0,34,36,0,17,32,0,0,11,8,0,3],
    [21,30,23,12,1,7,0,0,4,9,5,28,32,15,17,36,34,20,24,0,35,19,27,0,0,3,2,8,0,0,33,18,25,31,10,6],
    [0,0,29,0,13,27,7,0,12,23,1,21,14,8,11,2,3,22,26,5,0,28,16,0,0,6,33,31,0,10,36,0,17,15,32,0],
    [11,22,0,3,8,14,32,20,34,36,15,17,7,1,21,23,0,0,18,31,6,25,0,33,24,35,0,13,19,0,9,26,28,5,16,4],
    [17,20,0,34,0,32,0,18,0,0,31,25,16,5,28,9,4,0,22,8,3,11,14,2,0,0,23,1,21,0,29,0,19,0,0,35],
    [7,23,12,20,17,1,5,0,18,4,25,16,15,28,32,34,26,36,29,11,22,0,13,35,2,30,3,21,14,8,6,33,10,19,31,24],
    [32,36,34,26,28,0,31,33,24,6,19,10,5,25,16,4,18,9,2,0,30,14,8,3,0,0,12,0,7,1,35,29,0,11,13,22],
    [16,0,24,0,25,5,0,29,22,35,0,27,31,19,10,6,24,33,0,0,20,0,1,12,36,26,34,28,32,15,3,2,0,21,0,30],
    [14,2,3,30,21,8,15,0,26,0,28,32,1,17,7,12,20,23,0,19,24,10,0,6,0,0,0,11,0,13,0,9,0,25,5,0],
    [27,29,35,0,11,13,1,23,0,12,17,7,8,0,0,3,30,2,0,25,18,16,0,0,33,24,6,19,10,31,0,36,32,28,15,26],
    [10,33,6,24,19,31,8,2,30,3,21,14,13,11,27,35,22,0,36,28,26,32,15,34,9,0,4,25,0,5,12,23,0,17,1,20]]








-- Test cases without expected outcomes
testBoards :: [(String, Board)]
testBoards = [("Unique Board 1", uniqueBoard1),
              ("Ambiguous Board 1", invalidBoard16),
              ("Invalid Board 1", invalidBoard1),
              ("Unique Board C", uniqueBoardC),
              ("Ambiguous Board C",invalidBoard49),
              ("Invalid Board C", invalidBoardC)]

-- Run tests and print results
runFinalTests :: IO ()
runFinalTests = mapM_ testAndPrint testBoards
  where
    testAndPrint (name, board) = do
        let result = numSolutions board
        putStrLn $ name ++ ": " ++ show result

-- Main function to execute the test
main :: IO ()
main = runFinalTests