module WinConditions where

import Data.List
import Connect4Types

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