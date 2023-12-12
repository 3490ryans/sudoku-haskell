module SudokuSolver where

import Control.Monad
import Data.List (sortBy, delete, (\\))
import System.Random

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




type GenState = (Board, Coord, [Integer])
type Guess = (Coord, [Integer])

newNums :: Board -> GenState -> GenState
newNums curboard (board, coord, nums) = (board, coord, (delete n nums))
                                        where (Num n) = getCell curboard coord

goodNums :: Board -> Coord -> [Integer]
goodNums board c@(y,x) = let row = [n | Num n <- (getRow board y)]
                             column = [n | Num n <- (getColumn board x)]
                             square = foldr (\x y -> thrupleElems x ++ y) [] (thrupleElems $ getSquare board c)
                             squarecells = [n | Num n <- square]
                         in (([1..9] \\ row) \\ column) \\ squarecells

drawElement :: (Eq a) => [a] -> IO (Maybe a, [a])
drawElement [] = do
    return (Nothing, [])
drawElement xs = do
    index <- randomRIO (0,(length xs - 1))
    let item = xs !! index
    return (Just item, (delete item xs))

guessCoords :: Board -> [Coord] -> [Guess]
guessCoords board cs = zip cs [goodNums board c | c <- cs]

updateGuesses :: Board -> [Guess] -> [Guess]
updateGuesses board gs = guessCoords board (fst $ unzip gs)

compareGuesses :: Guess -> Guess -> Ordering
compareGuesses (c1, n1) (c2, n2) | length n1 < length n2 = LT
                                 | otherwise = GT

sortGuesses :: [Guess] -> [Guess]
sortGuesses gs = sortBy compareGuesses gs


tryGenerateCell :: GenState -> IO (Maybe Board)
tryGenerateCell g@(board, coord, nums) = do
    (n, ns) <- drawElement nums
    --putStrLn $ show n
    --putStrLn $ show g
    case n of
        Just i -> do
            let newBoard = updateBoard board coord (Num i)
            if (validBoard newBoard)
                then return (Just newBoard)
                else tryGenerateCell (board, coord, ns)
        Nothing -> return Nothing

generateCells :: Board -> [GenState] -> [Guess] -> IO Board
generateCells board [] [] = do
    return board
generateCells board [] (c:cs) = generateCells board [(board, fst c, snd c)] cs
generateCells board [g] cs = do
    attempt <- tryGenerateCell g
    case attempt of
        Just b -> if length cs > 0
                  then let (ng:ngs) = sortGuesses $ updateGuesses b cs
                       in  generateCells board ((b, fst ng, snd ng) : g : []) ngs
                  else return b
        Nothing -> return board
generateCells board (g:h:gs) cs = do
    attempt <- tryGenerateCell g
    case attempt of
        Just b -> if length cs > 0
                  then let (ng:ngs) = sortGuesses $ updateGuesses b cs
                       in  generateCells board ((b, fst ng, snd ng) : g : h : gs) ngs
                  else return b
        Nothing -> let ngs = delete (snd3 h, thd3 h) (sortGuesses $ updateGuesses (fst3 h) ((snd3 g, thd3 g) : cs))
                   in  generateCells board (newNums (fst3 g) h : gs) ngs

generateSolvedBoard :: IO Board
generateSolvedBoard = do
    generateCells emptyBoard [] (guessCoords emptyBoard allCoords)


solveBoard :: Board -> IO Board
solveBoard board = do
    generateCells board [] (sortGuesses $ guessCoords board (getEmptyCells board))


removeRandomCells :: Board -> Integer -> [Coord] -> IO Board
removeRandomCells board 0 _ = do
    return board
removeRandomCells board n coords | n >= 1 && n <= 81 = do
    (c,cs) <- drawElement coords
    case c of
        Just co -> removeRandomCells (updateBoard board co Empty) (n-1) cs
        Nothing -> return board

generatePuzzle :: Integer -> IO Board
generatePuzzle difficulty = do
    board <- generateSolvedBoard
    puzzleBoard <- removeRandomCells board difficulty allCoords
    return puzzleBoard