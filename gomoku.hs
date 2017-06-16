import Data.List
import Data.Char

boardSize = 19

data Board = Board {size :: Int, cells :: [[Point]]}

data Point = Point {x :: Int, y :: Int, color :: Color}

instance Eq Board where
    (Board _ cells1) == (Board _ cells2) = cells1 == cells2

instance Eq Point where
    (Point _ _ c1) == (Point _ _ c2) = c1 == c2

data Color = Black | White | Empty deriving Eq

instance Show Board where
    show (Board _ rows) = intercalate "\n" $ map show rows

instance Show Point where
    show (Point x y color) = show color

instance Show Color where
    show Empty = " _ "
    show Black = " o "
    show White = " x "

makeBoard :: Int -> Int-> [[Point]] -> [[Point]]
makeBoard x y list
    | x == -1 = list
    | otherwise = makeBoard (x-1) y (b:list)
    where b = makeColumns x y []

makeColumns :: Int -> Int -> [Point]-> [Point]
makeColumns x y list
    | y == -1 = list
    | otherwise = makeColumns x (y-1) (b:list)
    where b = Point x y Empty

insertFigure :: Board -> Int -> Int -> Color -> Board
insertFigure board x y figure
    | x > size board || y > size board || x < 0 || y < 0 = board
    | color (cells board !! x !! y) == White = board
    | color (cells board !! x !! y) == Black = board
    | color (cells board !! x !! y) == Empty = Board boardSize (insertFigureInRow board x y figure)

insertFigureInRow :: Board -> Int -> Int -> Color -> [[Point]]
insertFigureInRow board x y figure =
     let (a,b) = splitAt x (cells board)
        in let d = [insertFigureInColumn x y figure (head b)]
            in a ++ (d) ++ (tail b)

insertFigureInColumn :: Int -> Int -> Color -> [Point] -> [Point]
insertFigureInColumn x y figure list =
    let (a,b) = splitAt y list in (a ++ [Point x y figure] ++ (tail b))

checkResult :: Board -> Color -> Bool
checkResult board color =  
    checkBoard (cells board) (Point 1 1 color)
    || checkBoard (rotate (cells board)) (Point 1 1 color) -- to check columns
    || checkBoard (diagonals (cells board)) (Point 1 1 color) -- to check diagonals
    || checkBoard (diagonals (rotate (cells board))) (Point 1 1 color) -- to check diagonals
    

checkBoard :: (Eq x) => [[x]] -> x -> Bool
checkBoard [] _ = False
checkBoard (head:tail) color = do
    if (checkRow (-1) (elemIndices color head) 1) then
        True
    else do
        checkBoard tail color

checkRow :: Int -> [Int] -> Int -> Bool
checkRow _ [] sum
    | sum == 5 = True
    | otherwise = False
checkRow value (h:t) sum 
    | sum == 5 = True
    | value == (-1) = checkRow h t 1
    | h == (value + 1) = checkRow h t (sum + 1)
    | h /= (value + 1) = checkRow h t 1

diagonals :: [[a]] -> [[a]] -- love stack
diagonals = tail . go [] where
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]

rotate :: [[a]] -> [[a]]
rotate = transpose . reverse

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

computerMove :: Board -> Int -> Int -> Color -> IO()
computerMove board a b color = do
    let boardAfterMove = insertFigure board a b color
    if (boardAfterMove == board && b >= boardSize) then
        computerMove board (a + 1) 0 color
    else do
        if (boardAfterMove == board) then
           computerMove board a (b + 1) color
        else do
            putStrLn $ show boardAfterMove
            checkGame boardAfterMove color (checkResult boardAfterMove color)

playerMove :: Board -> Color -> IO()
playerMove board color = do
    putStr "Podaj wiersz: "
    x <- getLine
    if (isInteger x) then do    
        putStr "Podaj kolumnę: "
        y <- getLine
        if (isInteger y) then do
            let boardAfterMove = insertFigure board (read x::Int) (read y::Int) color
            if (boardAfterMove == board) then do
                putStrLn "Zła pozycja, spróbuj jeszcze raz!"
                playerMove board color
            else do
                putStrLn $ show boardAfterMove
                checkGame boardAfterMove color (checkResult boardAfterMove color)
        else do
            putStrLn "Zła wartość, spróbuj jeszcze raz"
            playerMove board color
    else do
        putStrLn "Zła wartość, spróbuj jeszcze raz"
        playerMove board color

checkGame :: Board -> Color -> Bool -> IO()
checkGame board color isGameOver = do
    if(isGameOver) then
        gameOver board color
    else do
        if (color == Black) then do
            let color2 = White
            putStrLn "\n"
            putStrLn "Twój ruch!"
            playerMove board color2
        else do
            let color2 = Black
            putStrLn "\n"
            putStrLn "Ruch komputera"
            computerMove board 0 0 color2

gameOver :: Board -> Color -> IO()
gameOver board1 color = do
    putStrLn "\n"
    putStrLn $ show board1
    putStrLn ""
    putStr $ show color
    putStrLn "jest zwyciezcą!"

startGame = playerMove (Board boardSize (makeBoard (boardSize - 1) (boardSize - 1) [])) (White)

