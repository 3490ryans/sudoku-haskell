import System.Exit (exitSuccess)

main :: IO ()
main = do
    putStrLn "Sudoku Menu:"
    putStrLn "1. Generate a new Sudoku Puzzle"
    putStrLn "2. Load a .sudoku File"
    putStrLn "Q. Quit"
    putStr "Choose an option: "
    choice <- getLine
    handleChoice choice

handleChoice :: String -> IO ()
handleChoice "1" = do
    -- Put instructions to generate sudoku Puzzle here
    putStrLn "A"    -- Replace this line (currently placeholder)
    subMenu
    main

handleChoice "2" = do
    -- Put instructions on how to load .sudoku file here
    putStrLn "B"    -- Replace this line (currently placeholder)
    subMenu
    main

handleChoice "Q" = do
    putStrLn "Quitting..."
    exitSuccess

handleChoice _ = do
    putStrLn "Invalid choice. Please choose again."
    main

subMenu :: IO ()
subMenu = do
    putStrLn "Submenu:"
    putStrLn "1. Generate a new Sudoku Puzzle"
    putStrLn "2. Load a .sudoku File"
    putStrLn "3. Solve Sudoku Puzzle"
    putStrLn "4. Check Sudoku Puzzle"
    putStrLn "5. Save Sudoku Puzzle"
    putStrLn "Q. Quit"
    putStr "Choose an option: "
    choice <- getLine
    handleSubChoice choice

handleSubChoice :: String -> IO ()
handleSubChoice "1" = do
    -- Put instructions to generate sudoku Puzzle here
    putStrLn "A"    -- Replace this line (currently placeholder)
    subMenu

handleSubChoice "2" = do
    -- Put instructions on how to load .sudoku file here
    putStrLn "B"    -- Replace this line (currently placeholder)
    subMenu

handleSubChoice "3" = do
    -- Instructions to solve sudoku puzzle here
    putStrLn "C"    -- Replace this line (currently placeholder)
    subMenu

handleSubChoice "4" = do
    -- Instructions to check sudoku puzzle here
    putStrLn "D"    -- Replace this line (currently placeholder)
    subMenu

handleSubChoice "5" = do
    -- Instructions to save Sudoku Puzzle here
    putStrLn "E"    -- Replace this line (currently placeholder)
    subMenu

handleSubChoice "Q" = do
    putStrLn "Quitting..."
    exitSuccess

handleSubChoice _ = do
    putStrLn "Invalid choice. Please choose again."
    subMenu
