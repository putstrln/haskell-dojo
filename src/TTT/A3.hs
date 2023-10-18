module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import Data.Bool (Bool(False))

-- Q#01
showInts :: [Int] -> [String]
showInts (x:xs) = show x : showInts xs
showInts [] = []

_HEADER_ = " " ++ formatLine (showInts _RANGE_)

-- Q#02
showSquares :: [Square] -> [String]
showSquares (x:xs) = show x : showSquares xs
showSquares [] = []

-- Q#03
formatRows :: [Row] -> [String]
formatRows (x:xs) = (formatLine . showSquares) x : formatRows xs
formatRows [] = []

-- Q#04
isColEmpty :: Row -> Int -> Bool
isColEmpty row i = go row i where
    go [] _ = False
    go (x:xs) 0 = x == Empty
    go (x:xs) currI = go xs (currI - 1)

-- Q#05

dropFirstCol :: Board -> Board
dropFirstCol = map tail

dropLastCol :: Board -> Board
dropLastCol = map init

-- Q#06
getDiag :: (Row -> Square) -> (Board -> Board) -> Board -> Line
getDiag _ _ [] = []
getDiag headOrTail dropFirstOrLastCol (x:xs) = headOrTail x : getDiag headOrTail dropFirstOrLastCol xs

getDiag1 :: Board -> Line
getDiag1 = getDiag head dropFirstCol

getDiag2 :: Board -> Line
getDiag2 = getDiag last dropLastCol

getAllLines :: Board -> [Line]
getAllLines xs = xs ++ transpose xs ++ [getDiag1 xs] ++ [getDiag2 xs]

-- Q#07
putSquare :: Player -> Board -> Move -> Board
putSquare p rs (rI, cI) = go rs rI where
    go (x:xs) 0 = replaceSquareInRow p rI x : xs
    go (x:xs) _ = x : go xs (rI - 1)
    go [] _ = []

-- Q#08
prependRowIndices :: [String] -> [String]
prependRowIndices xs = go (indexRowStrings xs) where
    go ((c, s):xxs) = (c : s) : go xxs
    go [] = []

-- Q#09
isWinningLine :: Player -> Line -> Bool
isWinningLine p l = isWinningLine_ False l where
    isWinningLine_ winning (x:xs) =
        if x == p
            then isWinningLine_ True xs
            else False
    isWinningLine_ winning [] = winning


-- Q#10
isValidMove :: Board -> Move -> Bool
isValidMove b m@(rI, cI) =
    if isMoveInBounds m
        then isValidMove_ b rI
        else False
    where
        isValidMove_ (r:rs) 0 = isColEmpty r cI
        isValidMove_ (r:rs) _ = isValidMove_ rs (rI - 1)
        isValidMove_ [] _ = False