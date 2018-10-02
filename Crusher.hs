module Crusher where

import Util
import CrusherBoard
import Piece
import AI

-- CONSTANTS {

boardsize b = div (length b + 1) 2

yes   = ["y", "yes", "ye"]
move  = ["m", "mo", "mov", "move"]
nums  = "0123456789"
-- TODO: write down rules here
rules = "Rules: \n"
newturn =
    "=========================================================\n" ++
    "   #   # #### #   # #######    ####### #   # ###   #   #\n" ++
    "   ##  # #     # #     #          #    #   # #  #  ##  #\n" ++
    "   # # # ####   #      #          #    #   # ###   # # #\n" ++
    "   #  ## #     # #     #          #    #   # #  #  #  ##\n" ++
    "   #   # #### #   #    #          #     ###  #   # #   #\n" ++
    "========================================================="
gameover =
    "==========================================================\n" ++
    "    ##     #   #   # ####     ###  #       # #### ###\n" ++
    "   #  #   # #  ## ## #       #   #  #     #  #    #  #\n" ++
    "   #      ###  # # # ####    #   #   #   #   #### ###\n" ++
    "   #  ## #   # #   # #       #   #    # #    #    #  #\n" ++
    "    ##   #   # #   # ####     ###      #     #### #   #\n" ++
    "=========================================================="

-- } /CONSTANTS

-- TODO: write purposes for functions

-- this constant can be changed to W or B when you want to play against AI
ai = X

play :: Int -> IO Board
play n =
    do
        putStrLn "Are you ready to play Crusher?"
        input <- getLine
        let ans = fixdel input
        if (elem ans yes)
            then do
                putStrLn rules
                doturn [] (generate n) W
            else do
                putStrLn "Oh, okay..."
                return baseState

doturn :: [Board] -> Board -> Piece -> IO Board
doturn visited board p =
    do
        if (ai == p)
            then do
                doaiturn visited board p
            else do
                doplayerturn visited board p

-- TODO: create AI
doaiturn :: [Board] -> Board -> Piece -> IO Board
doaiturn bs (NewBoard board) p =
    do
        let ps = countall board p
        if (ps > 0)
            then do
                let newboard = chooseplay bs (NewBoard board) p
                if (newboard == (NewBoard board))
                    then do
                        finalboard <- displaywinner (NewBoard board) p
                        return finalboard
                    else do
                        doturn ((NewBoard board):bs) newboard (getotherplayer p)
            else do
                finalboard <- displaywinner (NewBoard board) p
                return finalboard

displaywinner :: Board -> Piece -> IO Board
displaywinner board p =
    do
        putStrLn gameover
        putStrLn ("The winner is " ++ (getdisplaychar (getotherplayer p)) ++ "!")
        return board


doplayerturn :: [Board] -> Board -> Piece -> IO Board
doplayerturn visited (NewBoard board) p =
    do
        putStrLn "Here is the current state:"
        putStrLn (getdisplaystring board (boardsize board  - 1))
        let ps = countall board p
        if (ps > 0)
            then do
                putStrLn newturn
                putStrLn ("It is " ++ (getdisplaychar p) ++ "'s turn!")
                putStrLn "Which piece would you like to play?"
                putStrLn "(select a number on the board)"
                let (numboard, nummap) = placepiecemarkers board p
                putStrLn (getdisplaystring numboard (boardsize board - 1))
                input <- getLine
                let chosenpiece = fixdel input
                if (all (\ l -> elem l nums) chosenpiece)
                    then do
                        let ans = numtocoords ((read chosenpiece) :: Int) nummap
                        doturnmovestomp visited (NewBoard board) p ans
                    else do
                        putStrLn "That is not a number. Restarting turn."
                        newboard <- doturn visited (NewBoard board) p
                        return newboard
            else do
                finalboard <- displaywinner (NewBoard board) p
                return finalboard

doturnmovestomp :: [Board] -> Board -> Piece -> (Int, Int) -> IO Board
doturnmovestomp visited (NewBoard board) p chosen =
    do
        let (y, x) = chosen
        if (x >= 0 && y >= 0)
            then do
                putStrLn "You have selected a piece."
                putStrLn "Would you like to move or stomp?"
                putStrLn "(write 'm' or 's')"
                input <- getLine
                let ans = fixdel input
                if (elem ans move)
                    then do
                        newboard <- doturnmove visited (NewBoard board) p chosen
                        return newboard
                    else do
                        newboard <- doturnstomp visited (NewBoard board) p chosen
                        return newboard
            else do
                putStrLn "That number is not valid. Restarting turn."
                newboard <- doturn visited (NewBoard board) p
                return newboard

doturnmove :: [Board] -> Board -> Piece -> (Int, Int) -> IO Board
doturnmove visited (NewBoard board) p chosen =
    do
        let size = boardsize board
        putStrLn "Where would you like to move?"
        putStrLn "(select a number on the board)"
        let (moves, nummap) = getmoves board size chosen
        let newboard = moves
        putStrLn (getdisplaystring (placeusermarker newboard chosen) (size - 1))
        ans <- getLine
        let choice = fixdel ans
        if (all (\ l -> elem l nums) choice)
            then do
                let choiceInt = (read choice) :: Int
                if (choiceInt < 0 || choiceInt > length nummap)
                    then do
                        putStrLn "That is not a valid number. Restarting turn."
                        redoboard <- doturn visited (NewBoard board) p
                        return redoboard
                    else do
                        let newpos = numtocoords choiceInt nummap
                        let newboard = domove board chosen newpos
                        if (elem (NewBoard newboard) visited)
                            then do
                                putStrLn "This board has already existed. Choose another one."
                                redoboard <- doturn visited (NewBoard board) p
                                return redoboard
                            else do
                                nextboard <- doturn ((NewBoard board):visited) (NewBoard newboard) (getotherplayer p)
                                return nextboard
            else do
                redoboard <- doturnmove visited (NewBoard board) p chosen
                return redoboard
        return (NewBoard board)

doturnstomp :: [Board] -> Board -> Piece -> (Int, Int) -> IO Board
doturnstomp visited (NewBoard board) p chosen =
    do
        let size = boardsize board
        putStrLn "Where would you like to stomp?"
        putStrLn "(select a number on the board)"
        let (stomps, nummap) = getstomps board p size chosen
        let newboard = stomps
        putStrLn (getdisplaystring (placeusermarker newboard chosen) (size - 1))
        ans <- getLine
        let choice = fixdel ans
        if (all (\ l -> elem l nums) choice)
            then do
                let choiceInt = (read choice) :: Int
                if (choiceInt < 0 || choiceInt > length nummap)
                    then do
                        putStrLn "That is not a valid number. Restarting turn."
                        redoboard <- doturn visited (NewBoard board) p
                        return redoboard
                    else do
                        let newpos = numtocoords choiceInt nummap
                        let newboard = domove board chosen newpos
                        if (elem (NewBoard newboard) visited)
                            then do
                                putStrLn "This board has already existed. Choose another one."
                                redoboard <- doturn visited (NewBoard board) p
                                return redoboard
                            else do
                                nextboard <- doturn ((NewBoard board):visited) (NewBoard newboard) (getotherplayer p)
                                return nextboard
            else do
                redoboard <- doturnmove visited (NewBoard board) p chosen
                return redoboard
        return (NewBoard board)
