module HM.A8 where

import Control.Monad (when)
import Data.Char (toUpper)
import HM.A6
import HM.A7 hiding (validateNoDict, validateWithDict)
import HM.Provided
import System.Directory (doesFileExist)

-- Q#01
getUpperChar :: IO Char
getUpperChar = toUpper <$> getChar

-- Q#02

_DICT_ = do
  fileExists <- doesFileExist _DICT_FILE_
  if fileExists then words <$> readFile _DICT_FILE_ else pure []

isDictNonEmpty :: IO Bool
isDictNonEmpty = not . null <$> _DICT_

-- Q#03
makeGameIfValid :: Either GameException Secret -> Either GameException Game
makeGameIfValid = (makeGame <$>)

-- Q#04
getDict :: IO (Maybe Dictionary)
getDict = toMaybe <$> isDictNonEmpty <*> _DICT_

-- Q#05

validateNoDict = (isValidLength =<<) . hasValidChars

validateWithDict = (validateNoDict >>) . isInDict

-- Q#06
playGame :: Game -> IO ()
playGame g = do
  promptGuess
  move <- getUpperChar
  _SPACE_
  case processTurn move g of
    Left ge -> case ge of
      GameOver -> print GameOver >> print (secret g)
      _ -> print ge
    Right g' -> do
      print g'
      if guess g' == secret g' then
        do
          putStrLn "Game has been won!"
      else
        playGame g'

-- Q#07
startGame :: (Secret -> Either GameException Secret) -> IO ()
startGame validator = do
  s <- setSecret
  case makeGameIfValid . validator $ s of
    Left ge -> print ge >> startGame validator
    Right g -> print g >> playGame g

-- Q#08

runHM :: IO ()
runHM = do
  maybeDict <- getDict
  case maybeDict of
    Just d -> startGame $ validateWithDict d
    Nothing -> do
      putStrLn "Missing dictionary! Continue without dictionary? [Y/N]"
      continue <- getUpperChar
      when (continue == 'Y') (startGame validateNoDict)
        