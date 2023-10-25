module TTT.A4 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import TTT.A3 (getAllLines, putSquare)

-- Q#01
_HEADER_ = " " ++ formatLine (map show _RANGE_)

-- Q#02
showSquares x = map show x

-- Q#03

dropFirstCol = map tail

-- Q#04

dropLastCol = map init

--Q#05
formatRows :: [Row] -> [String]
formatRows = map (\x -> formatLine . showSquares $ x)

-- Q#06
isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ _ [] = False
isWinningLine_ p l = null . filter (/= p) $ l

-- Q#07
isWinningLine :: Player -> Line -> Bool
isWinningLine _ [] = False
isWinningLine p l = null $ foldr (\e x -> if e /= p then e:x else x) [] l

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]
          
-- Q#08
hasWon :: Player -> Board -> Bool
hasWon p b = not . null $ filter (isWinningLine p) $ getAllLines b

-- Q#09
getGameState :: Board -> GameState
getGameState b
    | hasWon X b = X_WIN
    | hasWon O b = O_WIN
    | hasWon Empty b = IN_PROGRESS
    | otherwise = TIE

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove p b m = (getGameState updatedBoard, updatedBoard) where
    updatedBoard = putSquare p b m

-- Q#10
prependRowIndices :: [String] -> [String]
prependRowIndices = zipWith (:) ['A'..]

-- Q#11
formatBoard :: Board -> String
formatBoard = unlines . (:) _HEADER_ . prependRowIndices . formatRows