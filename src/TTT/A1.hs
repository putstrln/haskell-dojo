module TTT.A1 where

import Data.Char (toUpper)

-- Q#01

_SIZE_ :: Int
_SIZE_ = 3

-- Q#02

_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03

convertRowIndex :: Char -> Int
convertRowIndex = subtract 65 . fromEnum . toUpper

-- Q#04

_INVALID_MOVE_ :: Move
_INVALID_MOVE_ = (-1, -1)

-- Q#05

_SEP_ :: String
_SEP_ = "_|_"

-- Q#06

data Square = X | O | Empty deriving (Show, Eq)

-- Q#07

data GameState = X_WIN | O_WIN | TIE | IN_PROGRESS deriving Show

-- Q#08
type Player = Square
type Row = [Square]
type Line = [Square]
type Board = [Row]
type Move = (Int, Int)

-- Q#09

getFirstPlayer :: Bool -> Player
getFirstPlayer x = if x
                    then X
                    else O

getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ x
    | x = X
    | otherwise = O

-- Q#10

showGameState :: GameState -> String
showGameState x = case x of
    X_WIN -> show X_WIN
    O_WIN -> show O_WIN
    TIE -> show TIE
    IN_PROGRESS -> show IN_PROGRESS

-- Q#11

switchPlayer :: Player -> Player
switchPlayer x = case x of
    X -> O
    O -> X
    Empty -> Empty


-- Q#12

showSquare :: Square -> String
showSquare = show