module Connect4 where

import Data.List
import Connect4Types
import Data.Map (Map)

isTie :: Board -> Bool
isTie [] = True
isTie (h:t) 
 | checkTie h = isTie t
 | otherwise = False

checkTie :: Row -> Bool
checkTie row 
 | E `notElem` row = True
 | otherwise = False

checkWin :: Board -> Token
checkWin board 
 | checkWinToken board P1 = P1
 | checkWinToken board P2 = P2
 | otherwise = E

checkWinToken :: Board -> Token -> Bool
checkWinToken board token
 | horizontalCheck board token = True
 | verticalCheck (transpose board) token = True
 | otherwise = False

verticalCheck :: Board -> Token -> Bool
verticalCheck [] token = False
verticalCheck (h:t) token 
 | findConsecToken h token = True
 | otherwise = verticalCheck t token

horizontalCheck :: Board -> Token -> Bool
horizontalCheck [] token = False
horizontalCheck (h:t) token 
 | findConsecToken h token = True
 | otherwise = horizontalCheck t token

-- Find 4 consecutive player in a list 
findConsecToken :: Row -> Token -> Bool
findConsecToken (f:s:t:fo:ta) token
 | f == token && s == token && t == token && fo == token = True
 | otherwise = findConsecToken (s:t:fo:ta) token
findConsecToken (h:t) token = False

createboard :: Int -> Int -> Board
createboard rows cols 
    | rows > 0 = createrow cols : createboard (rows - 1) cols
    | otherwise = []


createrow :: Int -> Row
createrow cols
    | cols > 0 = E : createrow (cols - 1)
    | otherwise = []

getNum :: IO Int
getNum = readLn 

-- Main game loop
 -- To play the game, call main
main :: IO ()
main = do
    putStrLn "Welcome to Connect4"
    -- putStrLn "Please enter the chip character for Player 1: "
    -- p1 <- getChar
    -- putStrLn "Please enter the chip character for Player 2: "
    -- p2 <- getChar
    putStrLn "Please enter the number of rows on the gameboard."
    rows <- getNum
    putStrLn "Please enter the number of columns on the gameboard."
    cols <- getNum
    play (createboard rows cols) P1

play :: Board -> Token -> IO ()
play board token = do
    move <- (getmove board)
    let newBoard = insertNTimes 6 (read move::Int) token board
    printBoard (newBoard)
    if checkWinToken newBoard token then do 
        putStrLn("Player using token " ++ (showToken token):[] ++ " wins!") 
        else play newBoard (getOpponentToken token)

getNewBoard :: Board -> Int -> Token -> Board
getNewBoard (row:rob) col token = row:rob

getmove :: Board -> IO Int
getmove board = do
    putStrLn( "Please place a token in a column between 0 and " ++ show (length board))
    col <- getNum
    if col <= (length board) then return col else (getmove board)

printBoard :: Board -> IO ()
printBoard board = 
  putStrLn (unlines (map showRow board))
  where
   showRow = map showToken

getOpponentToken :: Token -> Token
getOpponentToken t
    | t == P1 = P2
    | t == P2 = P1
    | otherwise = E -- ?

showToken :: Token -> Char
showToken P1 = 'A'
showToken P2 = 'B'
showToken E = '.'




insertNTimes n m t b 
    |n==0 = play b t 
    |otherwise = 
      do
        let newBoard = makeMove n m t b
        if (newBoard == b)
         then do
            InsertNTimes (n-1) m t b
         else do
            return newBoard

			
			
makeMove :: Int -> Int -> Token -> Board -> Board
makeMove n m c (x:xs)
      |m==0 = (replacey n c x) : xs
      |otherwise = x : (makeMove n (m-1) c xs)
         where replacey n c (x:xs) 
                   |n==0 = if x==E then c : xs else E : xs
                   |otherwise = x : (replacey (n-1) c xs)
 

 
