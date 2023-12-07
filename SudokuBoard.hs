module SudokuBoard where

import Data.List
import Data.Char

data Cell = Num Integer | Empty
    deriving (Show,Eq)

type Square = (
        (Cell, Cell, Cell),
        (Cell, Cell, Cell),
        (Cell, Cell, Cell)
    )

emptySquare :: Square
emptySquare = (
        (Empty, Empty, Empty),
        (Empty, Empty, Empty),
        (Empty, Empty, Empty)
    )

type Row = [Cell]
type Column = [Cell]

type Board = (
        (Square, Square, Square),
        (Square, Square, Square),
        (Square, Square, Square)
    )

emptyBoard :: Board
emptyBoard = (
        (emptySquare, emptySquare, emptySquare),
        (emptySquare, emptySquare, emptySquare),
        (emptySquare, emptySquare, emptySquare)
    )

type Coord = (Char, Integer)

type Updates = [(Coord, Cell)]

-- returns the elemnts of a triple tuple as a list
thrupleElems :: (a,a,a) -> [a]
thrupleElems (x,y,z) = [x,y,z]

-- returns the first element of a triple tuple
fst3 :: (a,b,c) -> a
fst3 (x,y,z) = x

-- returns the second element of a triple tuple
snd3 :: (a,b,c) -> b
snd3 (x,y,z) = y

-- returns the third element of a triple tuple
thd3 :: (a,b,c) -> c
thd3 (x,y,z) = z

getBigY :: Board -> Char -> (Square,Square,Square)
getBigY (ss1,ss2,ss3) y | y `elem` "abc" = ss1
                        | y `elem` "def" = ss2
                        | y `elem` "ghi" = ss3

getBigX :: (Square,Square,Square) -> Integer -> Square
getBigX (s1,s2,s3) n | n `elem` [1,2,3] = s1
                     | n `elem` [4,5,6] = s2
                     | n `elem` [7,8,9] = s3

getSmallY :: Char -> Char
getSmallY y | y `elem` "adg" = 'a'
            | y `elem` "beh" = 'b'
            | y `elem` "cfi" = 'c'

getSmallX :: Integer -> Integer
getSmallX x | x `elem` [1,4,7] = 1
            | x `elem` [2,5,8] = 2
            | x `elem` [3,6,9] = 3

-- returns list of Squares in a Board
getSquares :: Board -> [Square]
getSquares board = foldr (\x y -> thrupleElems x ++ y) [] (thrupleElems board)

-- returns mini row from a single Square
getMiniRow :: Square -> Char -> [Cell]
getMiniRow (cs1,cs2,cs3) 'a' = thrupleElems cs1
getMiniRow (cs1,cs2,cs3) 'b' = thrupleElems cs2
getMiniRow (cs1,cs2,cs3) 'c' = thrupleElems cs3

-- returns Row from a board
getRow :: Board -> Char -> Row
getRow board c = foldr (\x y -> (getMiniRow x (getSmallY c)) ++ y) [] (thrupleElems $ getBigY board c)

-- returns list of Rows in a board
getRows :: Board -> [Row]
getRows board = [getRow board c | c <- ['a'..'i']]


-- returns mini column from a single Square
getMiniColumn :: Square -> Integer -> [Cell]
getMiniColumn square 1 = map fst3 (thrupleElems square)
getMiniColumn square 2 = map snd3 (thrupleElems square)
getMiniColumn square 3 = map thd3 (thrupleElems square)

-- returns Column from a board
getColumn :: Board -> Integer -> Column
getColumn board n = let bigCol = foldr (\x y -> (getBigX x n) : y) [] (thrupleElems board)
                    in  foldr (\x y -> getMiniColumn x (getSmallX n) ++ y) [] bigCol

-- returns list of Columns in a board
getColumns :: Board -> [Column]
getColumns board = [getColumn board n | n <- [1..9]]


updateCells :: (Cell,Cell,Cell) -> Integer -> Cell -> (Cell,Cell,Cell)
updateCells (c1,c2,c3) 1 n = (n, c2, c3)
updateCells (c1,c2,c3) 2 n = (c1, n, c3)
updateCells (c1,c2,c3) 3 n = (c1, c2, n)

updateSquare :: Square -> Coord -> Cell -> Square
updateSquare (cs1,cs2,cs3) ('a',x) n = ((updateCells cs1 x n), cs2, cs3)
updateSquare (cs1,cs2,cs3) ('b',x) n = (cs1, (updateCells cs2 x n), cs3)
updateSquare (cs1,cs2,cs3) ('c',x) n = (cs1, cs2, (updateCells cs3 x n))

updateSquares :: (Square,Square,Square) -> Coord -> Cell -> (Square,Square,Square)
updateSquares (s1,s2,s3) (y,x) n | x `elem` [1,2,3] = ((updateSquare s1 coord n), s2, s3)
                                 | x `elem` [4,5,6] = (s1, (updateSquare s2 coord n), s3)
                                 | x `elem` [7,8,9] = (s1, s2, (updateSquare s3 coord n))
                                 where coord = (y, getSmallX x)


updateBoard :: Board -> Coord -> Cell -> Board
updateBoard (ss1,ss2,ss3) (y,x) n | y `elem` "abc" = ((updateSquares ss1 coord n), ss2, ss3)
                                  | y `elem` "def" = (ss1, (updateSquares ss2 coord n), ss3)
                                  | y `elem` "ghi" = (ss1, ss2, (updateSquares ss3 coord n))
                                  where coord = (getSmallY y, x)

bulkUpdateBoard :: Board -> Updates -> Board
bulkUpdateBoard board [] = board
bulkUpdateBoard board (u:us) = bulkUpdateBoard newboard us
                               where newboard = updateBoard board (fst u) (snd u)
