module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Q#01

promptPlayer :: Player -> String
promptPlayer x = "Player " ++ show x ++ "'s turn: enter a row and column position (ex. A1)"

-- Q#02

_RANGE_ :: [Int]
_RANGE_ = [0.._SIZE_-1]

-- Q#03

isDigit :: Char -> Bool
isDigit = (`elem` ['0'..'9'])

readDigit :: Char -> Int
readDigit x = if isDigit x then read [x] else -1

-- Q#04

_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ Empty

_EMPTY_BOARD_ :: Board
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05

isTied :: Board -> Bool
isTied = all (Empty `notElem`)

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
    , [O, X, X]
    , [O, X, O]
    ]

-- Q#06

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings = zip ['A'..]

-- Q#07

formatLine :: [String] -> String
formatLine line = concat [_SEP_, intercalate _SEP_ line, _SEP_]

-- Q#08

isMoveInBounds :: Move -> Bool
isMoveInBounds (x, y) = x `elem` _RANGE_ && y `elem` _RANGE_

-- Q#09

stringToMove :: String -> Move
stringToMove [x, y] = (convertRowIndex x, readDigit y)
stringToMove _ = _INVALID_MOVE_

-- Q#10
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow _ _ [] = []
replaceSquareInRow player i row = firstHalf ++ [player] ++ tail secHalf
    where (firstHalf, secHalf) = splitAt i row        

rsX :: Int -> Row -> Row
rsX = replaceSquareInRow X

rsO :: Int -> Row -> Row
rsO = replaceSquareInRow O