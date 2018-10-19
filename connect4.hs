module Connect4 where

import Data.List
import Connect4Types
import WinConditions
import Data.Map (Map)

{-
  createboard takes a number of m rows and n columns as integers and 
  returns a Board with m rows and n columns
-}

createboard :: Int -> Int -> Board
createboard rows cols 
    | rows > 0 = createrow cols : createboard (rows - 1) cols
    | otherwise = []

{-
  createrow takes a number of n columns as an integer and returns a row
  of n columns
-}

createrow :: Int -> Row
createrow cols
    | cols > 0 = E : createrow (cols - 1)
    | otherwise = []

{-
  getNum is an interface for readLn that allows for Integer operations 
  on the immediate IO input
-}

getNum :: IO Int
getNum = readLn 

-- Main game loop
 -- To play the game, call main
main :: IO ()
main = do
    putStrLn "Welcome to Connect4"
    putStrLn "Please enter the number of rows on the gameboard."
    rows <- getNum
    putStrLn "Please enter the number of columns on the gameboard."
    cols <- getNum
    if rows > 0 && cols > 0 then do
        play (createboard rows cols) P1
    else do
        putStrLn("Rows and Columns should be bigger than 0") 
        main

{-
  play takes the current Board as state and the current player Token,
  and performs a single turn based on getmove. If the game is not finished
  in that turn, play is called recursively on the other player's token and
  updated Board state.
-}

play :: Board -> Token -> IO ()
play board token = do
    move <- (getmove board token)
    let newBoard = insertNTimes move ((length board)-1) token board
    if (newBoard == board) then do
        putStrLn("Invalid inputs") 
        play board token
        else do printBoard (newBoard) 
                if checkWinToken newBoard token then do 
                    putStrLn("Player using token " ++ (showToken token):[] ++ " wins!") 
                else if isTie newBoard then do
                    putStrLn("Board full! Draw") 
                else play newBoard (getOpponentToken token)

{-
  getmove acquires and validates the player's selected row and
  returns the resulting IO Int
-}
getmove :: Board -> Token -> IO Int
getmove board token = do
    putStrLn("Player " ++ (showToken token):[] ++ ", please place a token in a column between 0 and " ++ show ((length board)-1))
    col <- getNum
    if col <= (length board) then return col else (getmove board token)

{-
  getBoard prints the UI of the gameboard each turn
-}
printBoard :: Board -> IO ()
printBoard board = 
  putStrLn (unlines (map showRow board))
  where
   showRow = map showToken

{-
  getOpponentToken takes the current player's Token and returns
  the Token of the opposing player
-}
getOpponentToken :: Token -> Token
getOpponentToken t
    | t == P1 = P2
    | t == P2 = P1

{-
  showToken maps player Token enums to Char's that are able to be
  rendered in the UI
-}
showToken :: Token -> Char
showToken P1 = 'A'
showToken P2 = 'B'
showToken E =  '.'

{-
  insertNTimes takes the number of nth column, mth row, token t
  and board b and returns the new board if that move changed the 
  board state, otherwise tries to perform a move on the next row
  up
-}
insertNTimes n m t b 
    |m == -1 = b
    |otherwise = 
      do
        let newBoard = makeMove n m t b
        if (newBoard == b)
         then do
            insertNTimes n (m-1) t b
         else do
            newBoard

{-
  makeMove takes the number of nth column, mth row, token c
  and board x:xs, and places the token c in the appropriate
  row and column.
-}
makeMove :: Int -> Int -> Token -> Board -> Board
makeMove n m c (x:xs)
      |m==0 = (replacey n c x) : xs
      |otherwise = x : (makeMove n (m-1) c xs)
         where 
          replacey n c [] = []
          replacey n c (x:xs) 
                   |n==0 = if x==E then c : xs else x : xs
                   |otherwise = x : (replacey (n-1) c xs)
 

 
