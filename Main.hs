--- CS3490 Programming Langauges: Final Project
--- Rou Yan Ling
--- Ryan Densmore
import Data.Char

import SudokuBoard


generateCoords :: String -> [Coord]
generateCoords s = take (length s) [(y,x) | y <- ['a'..'i'], x <- [1..9]]

parseCell :: Char -> Cell
parseCell c | c `elem` ['1'..'9'] = Num (toInteger $ digitToInt c)
            | c == '0'            = Empty

parseBoard :: String -> Board
parseBoard s = bulkUpdateBoard emptyBoard updates 
               where updates = zip (generateCoords s) (foldr (\c cs -> (parseCell c) : cs) [] s)


-- Printing
topBorder    = "╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗\n"
intraBorder  = "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢\n"
midBorder    = "╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣\n"
bottomBorder = "╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝\n"

printCellPretty :: Cell -> String
printCellPretty (Num n) = show n
printCellPretty (Empty) = " "

printCellsPretty :: (Cell,Cell,Cell) -> String
printCellsPretty (c1,c2,c3) = (printCellPretty c1) ++ " │ " ++ (printCellPretty c2) ++ " │ " ++ (printCellPretty c3)

printSquareRowPretty :: Square -> Char -> String
printSquareRowPretty (cs1,_,_) 'a' = printCellsPretty cs1
printSquareRowPretty (_,cs2,_) 'b' = printCellsPretty cs2
printSquareRowPretty (_,_,cs3) 'c' = printCellsPretty cs3

printRowPretty :: (Square,Square,Square) -> Char -> String
printRowPretty (s1,s2,s3) c = "║ " ++ (printSquareRowPretty s1 c) ++ " ║ " ++ (printSquareRowPretty s2 c) ++ " ║ " ++ (printSquareRowPretty s3 c) ++ " ║\n"

printSquaresPretty :: (Square,Square,Square) -> String
printSquaresPretty s = (printRowPretty s 'a') ++
                       intraBorder ++
                       (printRowPretty s 'b') ++
                       intraBorder ++
                       (printRowPretty s 'c')

printBoardPretty :: Board -> String
printBoardPretty (ss1,ss2,ss3) = topBorder ++
                                 (printSquaresPretty ss1) ++
                                 midBorder ++
                                 (printSquaresPretty ss2) ++
                                 midBorder ++
                                 (printSquaresPretty ss3) ++
                                 bottomBorder

exampleBoard = "000000013400200000600000000000460500010000007200500000000031000000000420080000000"

main :: IO ()
main = do
    let newboard = parseBoard exampleBoard
    putStr $ printBoardPretty newboard