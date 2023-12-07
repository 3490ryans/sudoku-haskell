module SudokuSolver where

import System.Random
import System.Random.Shuffle

import SudokuBoard

hasNoDups :: [Cell] -> Bool
hasNoDups [] = True
hasNoDups [x] = True
hasNoDups (Empty:xs) = True && (hasNoDups xs)
hasNoDups (x:y:xs) = (x /= y) && hasNoDups (x:xs) && hasNoDups (y:xs)


-- validates a single Square
validSquare :: Square -> Bool
validSquare s = let cells = foldr (\x y -> thrupleElems x ++ y) [] (thrupleElems s)
                in  hasNoDups cells

-- Checks if board is valid, ie no repeating numbers in each square, row, and column.
-- Empty cells are ok.
validBoard :: Board -> Bool
validBoard board = let squares = getSquares board
                       rows = getRows board
                       columns = getColumns board
                   in  and $ (map validSquare squares) ++ (map hasNoDups rows) ++ (map hasNoDups columns)


seed :: Int
seed = 257893

numList :: [Integer]
numList = [1..9]

generator = mkStdGen seed

-- some sort of draw random element function
-- shuffle' [1..9] 9 generator

-- generate a solved board
