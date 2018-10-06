module Connect4Types where

type Board = [Row]
type Row = [Token]
data Token = P1 | P2 | E
 deriving (Ord, Eq, Show)
