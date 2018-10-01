module Crusher where

import Util
import CrusherBoard
import Piece

-- CONSTANTS {

boardsize = 4
yes   = ["y", "yes", "ye"]
move  = ["m", "mo", "mov", "move"]
nums  = "0123456789"
rules = "Rules: \n"

-- } /CONSTANTS

play :: IO [[Piece]]
play =
    do
        putStrLn "Are you ready to play?"
        input <- getLine
        let ans = fixdel input
        if (elem ans yes)
            then do
                putStrLn rules
                doturn (generate boardsize) W
            else do
                putStrLn "Oh, okay..."
                return baseState

doturn :: [[Piece]] -> Piece -> IO [[Piece]]
doturn board p =
    do
        putStrLn ("It is " ++ (getdisplaychar p) ++ "'s turn!")
        putStrLn "(type 'x' at any time to restart the turn)"
        putStrLn "Which piece would you like to move?"
        putStrLn "(select a number on the board)"
        putStrLn (getdisplaystring (placepiecemarkers board p 1) (boardsize - 1))
        input <- getLine
        let chosenpiece = fixdel input
        if (chosenpiece == "x")
            then do
                newboard <- doturn board p
                return newboard
            else do
                if (all (\ l -> elem l nums) chosenpiece)
                    then do
                        let ans = (read chosenpiece) :: Int
                        doturnmovestomp board p ans
                    else do
                        putStrLn "That is not a number. Restarting turn."
                        newboard <- doturn board p
                        return newboard

doturnmovestomp :: [[Piece]] -> Piece -> Int -> IO [[Piece]]
doturnmovestomp board p ans =
    do
        if (ans > 0 && ans <= (countall board p))
            then do
                putStrLn "You have selected the piece called 'P':"
                putStrLn (getdisplaystring (placeuser board ans p) (boardsize - 1))
                putStrLn "Would you like to move or stomp?"
                input <- getLine
                let ans = fixdel input
                if (ans == "x")
                    then do
                        putStrLn "Restarting turn."
                        newboard <- doturn board p
                        return newboard
                    else do
                        if (elem ans move)
                            then do
                                putStrLn "Where would you like to move?"
                                putStrLn "(select a number on the board)"
                                putStrLn "This is the end of the implementation so far..."
                                return board
                            else do
                                putStrLn "Where would you like to stomp?"
                                putStrLn "(select a number on the board)"
                                putStrLn "This is the end of the implementation so far..."
                                return board
            else do
                putStrLn "That number is not valid. Restarting turn."
                newboard <- doturn board p
                return newboard

go = play
