module Connect4 where

import Connect4Types
import Data.Map (Map)

createboard :: Int -> Int -> [[Token]]
createboard rows cols 
    | rows > 0 = createrow cols : createboard (rows - 1) cols
    | otherwise = []


createrow :: Int -> [Token]
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
    putStrLn "Please enter the chip character for Player 1."
    --p1 <- getChar
    putStrLn "Please enter the chip character for Player 2."
    --p2 <- getChar
    putStrLn "Please enter the number of rows on the gameboard."
    rows <- getNum
    putStrLn "Please enter the number of columns on the gameboard."
    cols <- getNum
    printBoard (createboard rows cols)

printBoard :: Board -> IO ()
printBoard board =
  putStrLn (unlines (map showRow board))
  where
   showRow = map showToken

showToken :: Token -> Char
showToken P1 = 'A'
showToken P2 = 'B'
showToken E = '.'

