module TTT.A5 where

import Control.Monad (when)
import System.Random.Stateful (globalStdGen, uniformM)
import TTT.A1
import TTT.A2
import TTT.A3
import TTT.A4

-- Q#01
printBoard :: Board -> IO ()
printBoard = putStrLn . formatBoard

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/ttt-logo.txt"

printLogo :: IO ()
printLogo = readFile _LOGO_PATH_ >>= putStrLn

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player
firstPlayer = _RANDOM_BOOL_ >>= (return . getFirstPlayer)

-- Q#04
getMove :: Board -> IO Move
getMove b = getLine >>=
    return .stringToMove >>= \m ->
        if isValidMove b m then return m
        else
            putStrLn "Invalid move! Try again" >>
            getMove b

-- Q#05
play :: Board -> Player -> IO ()
play b p =
    when _DISPLAY_LOGO_ printLogo >>
    printBoard b >>
    putStrLn (promptPlayer p) >>
    getMove b >>=
    (\m -> return $ playMove p b m) >>= \(state, newBoard) ->
        if state == IN_PROGRESS then
            play newBoard (switchPlayer p)
        else
            printBoard newBoard >>
            putStrLn (showGameState state)

-- Q#06

runTTT :: IO ()
runTTT = firstPlayer >>= play _EMPTY_BOARD_

-- Q#07

printLogoDo = do
    content <- readFile _LOGO_PATH_
    putStrLn content

-- Q#08

firstPlayerDo = do
    return getFirstPlayer _RANDOM_BOOL_

-- Q#09

getMoveDo b = do
    input <- getLine
    let move = stringToMove input
    if isValidMove b move then
        do
            return move
    else
        do
            putStrLn "Invalid move! Try again"
            getMoveDo b

-- Q#10

playDo b p = do
    when _DISPLAY_LOGO_ printLogo
    printBoard b
    putStrLn $ promptPlayer p
    move <- getMove b
    (state, newBoard) <- return $ playMove p b move
    if state == IN_PROGRESS then
        play newBoard (switchPlayer p)
    else
        do
            printBoard newBoard
            putStrLn $ showGameState state