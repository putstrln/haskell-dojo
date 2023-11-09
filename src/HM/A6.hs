module HM.A6 where

import Data.Char (isAlpha)
import HM.Provided

-- Q#01
type Chances = Int
type Guess = String
type Move = Char
type Secret = String
type Dictionary = [String]

-- Q#02
data GameException =
    InvalidChars |
    InvalidLength |
    NotInDict |
    InvalidMove |
    RepeatMove |
    GameOver

-- Q#03
lengthInRange :: Secret -> Bool
lengthInRange s = read s >= fst _LENGTH_ && read s <= snd _LENGTH_ 

-- Q#04
invalidMove :: Move -> Bool
invalidMove = not . isAlpha

-- Q#05
revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters m s g = zipWith (\cs cg -> if m == cs then m else cg) s g

-- Q#06
updateChances :: Move -> Secret -> Chances -> Chances
updateChances m s c = if m `elem` s then c else c - 1

-- Q#07
setSecret :: IO String
setSecret = do
    putStr "Enter a secret word:\t"
    showInput False
    input <- getLine
    showInput True
    _SPACE_
    return input

