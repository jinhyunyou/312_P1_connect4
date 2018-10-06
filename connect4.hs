module Connect4 where

type Board = [Row]
type Row = [Player]
data Player = O | B | X
 deriving (Ord, Eq, Show)

createboard :: Int -> Int -> [[Player]]
createboard rows cols 
    | rows > 0 = createrow cols : createboard (rows - 1) cols
    | otherwise = []


createrow :: Int -> [Player]
createrow cols
    | cols > 0 = B : createrow (cols - 1)
    | otherwise = []

-- Main game loop

 -- To play the game, call main
main :: IO ()
main = do
    putStrLn "Welcome to Connect4"
    printBoard (createboard 6 7)

printBoard :: Board -> IO ()
printBoard board =
  putStrLn (unlines (map showRow board))
  where
   showRow = map showPlayer

showPlayer :: Player -> Char
showPlayer O = 'O'
showPlayer B = '.'
showPlayer X = 'X'
