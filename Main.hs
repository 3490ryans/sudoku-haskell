--- CS3490 Programming Langauges: Final Project
--- Rou Yan Ling
--- Ryan Densmore
import Data.Char

import SudokuBoard
import Control.Exception
import System.Exit (exitSuccess)
import SudokuSolver
import SudokuSolver (validBoard, solveBoard)



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
    return (parseBoard content)

boardToString :: Board -> String
boardToString (rows1, rows2, rows3) =
    concatMap getRows [rows1, rows2, rows3]
  where
    getRows :: (Square, Square, Square) -> String
    getRows (s1, s2, s3) =
        concatMap getRow ['a', 'b', 'c']
      where
        getRow :: Char -> String
        getRow y =
            printSquareRowPretty s1 y ++ " " ++
            printSquareRowPretty s2 y ++ " " ++
            printSquareRowPretty s3 y ++ "\n"


exampleBoard = "000000013400200000600000000000460500010000007200500000000031000000000420080000000"

main :: IO ()
main = do
    putStrLn "Sudoku Menu:"
    putStrLn "1. Generate a new Sudoku Puzzle"
    putStrLn "2. Load a .sudoku File"
    putStrLn "Q. Quit"
    putStr "Choose an option: "
    choice <- getLine
    handleChoice choice emptyBoard  -- Pass emptyBoard as the initial board

handleChoice :: String -> Board -> IO ()
handleChoice "1" currentBoard = do
    -- Put instructions to generate sudoku Puzzle here
    putStrLn "Generating a new Sudoku Puzzle..."
    newBoard <- generatePuzzle 5
    putStrLn $ printBoardPretty newBoard
    subMenu newBoard

handleChoice "2" currentBoard = do
    -- Put instructions on how to load .sudoku file here
    putStrLn "Enter the path to your Sudoku board file (.sudoku):"
    filePath <- getLine

    result <- try (readBoardFromFile filePath) :: IO (Either SomeException Board)
    case result of
        Left e -> putStrLn $ "Error reading the Sudoku board: " ++ show e
        Right newBoard -> do
            putStrLn $ printBoardPretty newBoard
            putStrLn $ "Sudoku board loaded from file: " ++ filePath
            subMenu newBoard

handleChoice "Q" _ = do
    putStrLn "Quitting..."
    exitSuccess

handleChoice _ currentBoard = do
    putStrLn "Invalid choice. Please choose again."
    main

subMenu :: Board -> IO ()
subMenu currentBoard = do
    putStrLn "Submenu:"
    putStrLn "1. Generate a new Sudoku Puzzle"
    putStrLn "2. Load a .sudoku File"
    putStrLn "3. Solve Sudoku Puzzle"
    putStrLn "4. Check Sudoku Puzzle"
    putStrLn "5. Save Sudoku Puzzle"
    putStrLn "Q. Quit"
    putStr "Choose an option: "
    choice <- getLine
    handleSubChoice choice currentBoard

handleSubChoice :: String -> Board -> IO ()
handleSubChoice "1" currentBoard = do
    -- Put instructions to generate sudoku Puzzle here
    putStrLn "Generating a new Sudoku Puzzle..."
    newBoard <- generatePuzzle 5
    putStrLn $ printBoardPretty newBoard
    subMenu newBoard

handleSubChoice "2" currentBoard = do
    -- Put instructions on how to load .sudoku file here
    putStrLn "Enter the path to your Sudoku board file (.sudoku):"
    filePath <- getLine

    result <- try (readBoardFromFile filePath) :: IO (Either SomeException Board)
    case result of
        Left e -> putStrLn $ "Error reading the Sudoku board: " ++ show e
        Right newBoard -> do
            putStrLn $ printBoardPretty newBoard
            putStrLn $ "Sudoku board loaded from file: " ++ filePath
            subMenu newBoard

handleSubChoice "3" currentBoard = do
    -- Put instructions to SOLVE sudoku Puzzle here
    solvedBoard <- solveBoard currentBoard
    putStrLn "Solved Sudoku Puzzle:"
    putStrLn $ printBoardPretty solvedBoard
    subMenu solvedBoard

handleSubChoice "4" currentBoard = do
    -- Put instructions to CHECK sudoku Puzzle here
    let isValid = validBoard currentBoard  
    subMenu currentBoard

handleSubChoice "5" currentBoard = do
    -- Put instructions to SAVE sudoku Puzzle here
    -- Saving Process
    putStrLn "Do you want to save the Sudoku board? Saving will stop the program!! (y/n)"
    userChoice <- getLine

    case userChoice of
        "y" -> do
            putStrLn "Enter the path to save your Sudoku board file (.sudoku):"
            savePath <- getLine

            let boardString = boardToString currentBoard

            -- Save the Sudoku board string to the specified file
            saveBoardToFile boardString savePath

            putStrLn $ "Sudoku board saved to file: " ++ savePath

        "n" -> putStrLn "Sudoku board not saved."

        _ -> do
            putStrLn "Invalid choice. Please enter 'y' or 'n'."
            subMenu currentBoard

handleSubChoice "Q" _ = do
    putStrLn "Quitting..."
    exitSuccess

handleSubChoice _ currentBoard = do
    putStrLn "Invalid choice. Please choose again."
    subMenu currentBoard
