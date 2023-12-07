module SudokuBoard where

data Cell = Num Integer | Empty
	deriving Show

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

type Row = (Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell)
type Column = (Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell)

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


updateCells :: (Cell,Cell,Cell) -> Integer -> Cell -> (Cell,Cell,Cell)
updateCells (c1,c2,c3) 1 n = (n, c2, c3)
updateCells (c1,c2,c3) 2 n = (c1, n, c3)
updateCells (c1,c2,c3) 3 n = (c1, c2, n)

updateSquare :: Square -> Coord -> Cell -> Square
updateSquare (cs1,cs2,cs3) ('a',x) n = ((updateCells cs1 x n), cs2, cs3)
updateSquare (cs1,cs2,cs3) ('b',x) n = (cs1, (updateCells cs2 x n), cs3)
updateSquare (cs1,cs2,cs3) ('c',x) n = (cs1, cs2, (updateCells cs3 x n))


getSmallX :: Integer -> Integer
getSmallX x | x `elem` [1,4,7] = 1
            | x `elem` [2,5,8] = 2
            | x `elem` [3,6,9] = 3

updateSquares :: (Square,Square,Square) -> Coord -> Cell -> (Square,Square,Square)
updateSquares (s1,s2,s3) (y,x) n | x `elem` [1,2,3] = ((updateSquare s1 coord n), s2, s3)
                                 | x `elem` [4,5,6] = (s1, (updateSquare s2 coord n), s3)
                                 | x `elem` [7,8,9] = (s1, s2, (updateSquare s3 coord n))
                                 where coord = (y, getSmallX x)


getSmallY :: Char -> Char
getSmallY y | y `elem` "adg" = 'a'
            | y `elem` "beh" = 'b'
            | y `elem` "cfi" = 'c'

updateBoard :: Board -> Coord -> Cell -> Board
updateBoard (ss1,ss2,ss3) (y,x) n | y `elem` "abc" = ((updateSquares ss1 coord n), ss2, ss3)
                                  | y `elem` "def" = (ss1, (updateSquares ss2 coord n), ss3)
                                  | y `elem` "ghi" = (ss1, ss2, (updateSquares ss3 coord n))
                                  where coord = (getSmallY y, x)

bulkUpdateBoard :: Board -> Updates -> Board
bulkUpdateBoard board [] = board
bulkUpdateBoard board (u:us) = bulkUpdateBoard newboard us
                               where newboard = updateBoard board (fst u) (snd u)
