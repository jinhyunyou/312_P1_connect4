module WinConditions where

import Data.List
import Connect4Types

-- Check tie condition (full board)
isTie :: Board -> Bool
isTie [] = True
isTie (h:t) 
 | checkTie h = isTie t
 | otherwise = False
-- Return true if row has no empty tile
checkTie :: Row -> Bool
checkTie row 
 | E `notElem` row = True
 | otherwise = False

-- Check all the possible winning condition (Vertical, Horizontal, Diagonal)
checkWinToken :: Board -> Token -> Bool
checkWinToken board token
 | horizontalCheck board token = True
 | verticalCheck (transpose board) token = True
 | diagonalCheck board token = True
 | otherwise = False

-- Check vertical winning condition
verticalCheck :: Board -> Token -> Bool
verticalCheck [] token = False
verticalCheck (h:t) token 
 | findConsecToken h token = True
 | otherwise = verticalCheck t token

-- Check horizontal winning condition
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

-- Diagonal check
diagonalCheck :: Board -> Token -> Bool
diagonalCheck board token 
 | checkAllDiagonals (getAllDiagonals board) token = True
 | otherwise = False

-- Recurse through all diagonals to check if someone is winning
checkAllDiagonals :: Board -> Token -> Bool
checkAllDiagonals [] token = False
checkAllDiagonals (h:t) token
 | checkDiagonal h token = True 
 | otherwise = checkAllDiagonals t token

-- Check a single diagonal if it contains 4 consecutive token
checkDiagonal :: [Token] -> Token -> Bool
checkDiagonal (f:s:t:fo:ta) token 
 | f == token && s == token && t == token && fo == token = True
 | otherwise = checkDiagonal (s:t:fo:ta) token
checkDiagonal (h:t) token = False

-- Reverse board to get opposit direction diagonals 
getReverseBoard :: Board -> Board
getReverseBoard [] = []
getReverseBoard board = map reverseRow board

-- Reverse row 
reverseRow :: Row -> Row
reverseRow row = reverse row

-- Get all diagonals from board (all directions)
getAllDiagonals :: Board -> [[Token]]
getAllDiagonals board = (diagonals board) ++ (diagonals (getReverseBoard board))

-- Get diagonals from board
diagonals :: Board -> [[Token]]
diagonals []       = []
diagonals ([]:board) = board
diagonals board      = zipWith (++) (map ((:[]) . head) board ++ repeat [])
                                  ([]:(diagonals (map tail board)))
