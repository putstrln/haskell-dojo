module HM.A7 where

import Data.Char (isAlpha, toLower, toUpper)
import HM.A6
import HM.Provided
import System.Directory (doesFileExist)
import Data.List (intersperse, sort)

-- Q#01
data Game = Game {
  secret :: String,
  guess :: String,
  moves :: [Char],
  chances :: Int
}

-- Q#02
repeatedMove :: Move -> Game -> Bool
repeatedMove m g = m `elem` secret g

-- Q#03
makeGame :: Secret -> Game
makeGame s = Game {
  secret = map toUpper s,
  guess  = '_' <$ s,
  {-- same as
  guess = const '_' <$> s
  guess = replicate (length s) '_'
  --}
  moves = [],
  chances = _CHANCES_
}

-- Q#04
updateGame :: Move -> Game -> Game
updateGame m g = g {
  guess = revealLetters m (secret g) (guess g),
  moves = m : moves g,
  chances = updateChances m (secret g) (chances g)
}

-- Q#05
instance Show Game where
  show (Game {secret = s, moves = m, chances = c}) = showGameHelper s m c

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper game moves chances =
  unlines
    [ _STARS_,
      "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n",
      "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n",
      "\tChances:\t" ++ show chances,
      _STARS_
    ]

-- Q#06
instance Show GameException where
  show InvalidChars = "InvalidChars"
  show InvalidLength = concat [lb, ub, "InvalidLength"]
    where
      lb = show $ fst _LENGTH_
      ub = show $ snd _LENGTH_
  show NotInDict = "NotInDict"
  show InvalidMove = "InvalidMove"
  show RepeatMove = "RepeatMove"
  show GameOver = "GameOver"

-- Q#07
toMaybe :: Bool -> a -> Maybe a
toMaybe cond a = if cond then Just a else Nothing

-- Q#08
validateSecret :: (Secret -> Bool) -> GameException -> Secret -> Either GameException Secret
validateSecret f ge s = if f s then Right s else Left ge

-- Q#09
hasValidChars = validateSecret (\s -> all isAlpha s) InvalidChars

isValidLength = validateSecret lengthInRange InvalidLength

isInDict :: Dictionary -> Secret -> Either GameException Secret
isInDict d = validateSecret ((`elem` d) . map toLower) NotInDict

-- Q#10
validateNoDict = (isValidLength =<<) . hasValidChars

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict d s = case validateNoDict s of
  Right _ -> Right s
  Left err -> Left err

-- Q#11
processTurn :: Move -> Game -> Either GameException Game
processTurn m g
  | invalidMove m = Left InvalidMove
  | repeatedMove m g = Left RepeatMove
  | chances updatedG < 0 = Left GameOver
  | otherwise = Right updatedG
    where
      updatedG = updateGame m g