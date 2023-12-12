--- CS3490 Programming Langauges: Final Project
--- Rou Yan Ling
--- Ryan Densmore
module SudokuMain (smain) where


import Data.Char
import Control.Exception
import System.IO

import SudokuBoard
import SudokuSolver



generateCoords :: String -> [Coord]
generateCoords s = take (length s) [(y,x) | y <- ['a'..'i'], x <- [1..9]]

parseCell :: Char -> String -> Cell
parseCell c v | c `elem` ['1'..'9'] = Num (toInteger $ digitToInt c)
              | c `elem` v = Empty

cleanString :: String -> String -> String
cleanString [] _ = []
cleanString (x:xs) v | x `elem` ['1'..'9'] = x : cleanString xs v
                     | x `elem` v = x : cleanString xs v
                     | otherwise  = cleanString xs v

parseBoard :: String -> String -> Board
parseBoard s v = let i = cleanString s v
                     updates = zip (generateCoords i) (foldr (\c cs -> (parseCell c v) : cs) [] i)
                 in bulkUpdateBoard emptyBoard updates


-- Printing
topBorder    = "╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗\n"
intraBorder  = "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢\n"
midBorder    = "╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣\n"
bottomBorder = "╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝\n"

printCellPretty :: Cell -> Char -> String
printCellPretty (Num n) _ = show n
printCellPretty (Empty) v = [v]

printCellsPretty :: (Cell,Cell,Cell) -> Char -> String
printCellsPretty (c1,c2,c3) v = (printCellPretty c1 v) ++ " │ " ++ (printCellPretty c2 v) ++ " │ " ++ (printCellPretty c3 v)

printSquareRowPretty :: Square -> Char -> Char -> String
printSquareRowPretty (cs1,_,_) 'a' v = printCellsPretty cs1 v
printSquareRowPretty (_,cs2,_) 'b' v = printCellsPretty cs2 v
printSquareRowPretty (_,_,cs3) 'c' v = printCellsPretty cs3 v

printRowPretty :: (Square,Square,Square) -> Char -> Char -> String
printRowPretty (s1,s2,s3) c v = "║ " ++ (printSquareRowPretty s1 c v) ++ " ║ " ++ (printSquareRowPretty s2 c v) ++ " ║ " ++ (printSquareRowPretty s3 c v) ++ " ║\n"

printSquaresPretty :: (Square,Square,Square) -> Char -> String
printSquaresPretty s v = (printRowPretty s 'a' v) ++
                         intraBorder ++
                         (printRowPretty s 'b' v) ++
                         intraBorder ++
                         (printRowPretty s 'c' v)

printBoardPretty :: Board -> Char -> String
printBoardPretty (ss1,ss2,ss3) v = topBorder ++
                                   (printSquaresPretty ss1 v) ++
                                   midBorder ++
                                   (printSquaresPretty ss2 v) ++
                                   midBorder ++
                                   (printSquaresPretty ss3 v) ++
                                   bottomBorder

-- saveBoardToFile :: String -> FilePath -> IO ()
saveBoardToFile :: String -> FilePath -> IO ()
saveBoardToFile boardString filePath = do
    result <- try (writeFile filePath boardString) :: IO (Either SomeException ())
    case result of
        Left e -> putStrLn $ "Error: " ++ show e
        Right _ -> putStrLn $ "Sudoku board saved to: " ++ filePath

-- Read Sudoku board from file
readBoardFromFile :: FilePath -> IO Board
readBoardFromFile filePath = do
    content <- readFile filePath
    return (parseBoard content ".0")



clearScreen :: IO ()
clearScreen = do
    putStr "\ESC[1J"
    putStr "\ESC[H"

playSudoku :: Board -> String -> IO ()
playSudoku board status = do
    clearScreen
    putStrLn "Add a number to a cell by addressing it via its coordinate, ie 'a4 5'"
    putStrLn "Type 'check' to check your solution, 'save' to save the board to a file for later, and 'solve' to solve the board!\n"
    putStrLn $ status ++ "\n"
    putStr $ printBoardPretty board ' '
    putStr "\n> "
    input <- getLine
    case input of
        "q" -> return ()
        "Q" -> return ()
        "check" -> if validBoard board
                   then playSudoku board "Looks good!"
                   else playSudoku board "Not quite!"
        "save" -> do
            putStrLn "Enter the path to save your Sudoku board file (.sudoku):"
            putStr "> "
            savePath <- getLine
            let boardString = printBoardPretty board '.'
            -- Save the Sudoku board string to the specified file
            saveBoardToFile boardString savePath
            putStrLn $ "Sudoku board saved to file: " ++ savePath
        "solve" -> do
            solvedBoard <- solveBoard board
            playSudoku solvedBoard "Solved!"
        (y:x:space:n:[]) | y `elem` ['a'..'i'] && x `elem` ['1'..'9'] && isSpace space && n `elem` ['1'..'9'] -> playSudoku (updateBoard board (y,(fromIntegral $ digitToInt x)) (Num (fromIntegral $ digitToInt n))) ""
                         | otherwise -> playSudoku board ""
        t -> playSudoku board ""

genSudokuMenu :: IO ()
genSudokuMenu = do
    clearScreen
    putStrLn "Let's play! Enter a difficulty, or number of missing cells."
    putStr "> "
    input <- getLine
    case input of
        "q" -> return ()
        "Q" -> return ()
        t | and $ map isDigit t -> let n = (read input :: Integer)
                                  in if n >= 0 && n <= 81
                                     then do
                                        board <- generatePuzzle n
                                        playSudoku board ""
                                     else genSudokuMenu
          | otherwise -> genSudokuMenu

loadSudokuMenu :: IO ()
loadSudokuMenu = do
    putStrLn "Enter the path to your Sudoku board file (.sudoku):"
    putStr "> "
    filePath <- getLine

    result <- try (readBoardFromFile filePath) :: IO (Either SomeException Board)
    case result of
        Left e -> putStrLn $ "Error reading the Sudoku board: " ++ show e
        Right newBoard -> do
            playSudoku newBoard "Loaded!"

introPrompt :: IO ()
introPrompt = do
    clearScreen
    putStrLn "Welcome to Sudoku! Press Q at any time to Quit."
    putStrLn "1. Generate a new Sudoku Puzzle"
    putStrLn "2. Load a .sudoku File"
    --hFlush stdout
    putStr "> "
    input <- getLine
    case input of
        "q" -> return ()
        "Q" -> return ()
        "1" -> genSudokuMenu
        "2" -> loadSudokuMenu
        otherwise -> introPrompt



smain :: IO ()
smain = do
    hSetBuffering stdout NoBuffering
    clearScreen
    introPrompt