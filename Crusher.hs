module Crusher where

import Util
import CrusherBoard
import Piece
import AI

-- CONSTANTS {

yes   = ["y", "yes", "ye"]
move  = ["m", "mo", "mov", "move"]
nums  = "0123456789"
-- TODO: write down rules here
rules = "Rules: \n"
newTurn =
    "=========================================================\n" ++
    "   #   # #### #   # #######    ####### #   # ###   #   #\n" ++
    "   ##  # #     # #     #          #    #   # #  #  ##  #\n" ++
    "   # # # ####   #      #          #    #   # ###   # # #\n" ++
    "   #  ## #     # #     #          #    #   # #  #  #  ##\n" ++
    "   #   # #### #   #    #          #     ###  #   # #   #\n" ++
    "========================================================="
gameOver =
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
ai = B

play :: Int -> IO Board
play n =
    do
        putStrLn "Are you ready to play Crusher?"
        input <- getLine
        let ans = fixDel input
        if (elem ans yes)
            then do
                putStrLn rules
                turn <- (doTurn [] (generate n) W)
                return turn
            else do
                putStrLn "Oh, okay..."
                return baseState

doTurn :: [Board] -> Board -> Piece -> IO Board
doTurn visited board p =
    do
        if (ai == p)
            then do
                nextTurn <- (doAiTurn visited board p)
                return nextTurn
            else do
                nextTurn <- (doPlayerTurn visited board p)
                return nextTurn

doAiTurn :: [Board] -> Board -> Piece -> IO Board
doAiTurn bs board p =
    do
        let ps = countAll board p
        if (ps > 0)
            then do
                let newboard = choosePlay bs board p
                if (newboard == board)
                    then do
                        putStrLn "Game over"
                        finalBoard <- displayWinner board p
                        return finalBoard
                    else do
                        putStrLn "Next turn"
                        nextBoard <- doTurn (board:bs) newboard (getOtherPlayer p)
                        return nextBoard
            else do
                finalBoard <- displayWinner board p
                return finalBoard

displayWinner :: Board -> Piece -> IO Board
displayWinner board p =
    do
        putStrLn gameOver
        putStrLn ("The winner is " ++ (getDisplayChar (getOtherPlayer p)) ++ "!")
        return board


doPlayerTurn :: [Board] -> Board -> Piece -> IO Board
doPlayerTurn visited board p =
    do
        putStrLn "Here is the current state:"
        putStrLn (getDisplayString board ((getSize board) - 1))
        let ps = countAll board p
        if (ps > 0)
            then do
                putStrLn newTurn
                putStrLn ("It is " ++ (getDisplayChar p) ++ "'s turn!")
                putStrLn "Which piece would you like to play?"
                putStrLn "(select a number on the board)"
                let (numBoard, numMap) = placePieceMarkers visited board p
                putStrLn (getDisplayString numBoard ((getSize board) - 1))
                input <- getLine
                let chosenPiece = fixDel input
                if (all (\ l -> elem l nums) chosenPiece)
                    then do
                        let ans = numToCoords ((read chosenPiece) :: Int) numMap
                        doTurnMoveStomp visited board p ans
                    else do
                        putStrLn "That is not a number. Restarting turn."
                        newBoard <- doTurn visited board p
                        return newBoard
            else do
                finalBoard <- displayWinner board p
                return finalBoard

doTurnMoveStomp :: [Board] -> Board -> Piece -> Point -> IO Board
doTurnMoveStomp visited board p chosen =
    do
        let (y, x) = chosen
        if (x >= 0 && y >= 0)
            then do
                putStrLn "You have selected a piece."
                putStrLn "Would you like to move or stomp?"
                putStrLn "(write 'm' or 's')"
                input <- getLine
                let ans = fixDel input
                if (elem ans move)
                    then do
                        newBoard <- doTurnMove visited board p chosen
                        return newBoard
                    else do
                        newBoard <- doTurnStomp visited board p chosen
                        return newBoard
            else do
                putStrLn "That number is not valid. Restarting turn."
                newBoard <- doTurn visited board p
                return newBoard

doTurnMove :: [Board] -> Board -> Piece -> Point -> IO Board
doTurnMove visited board p chosen =
    do
        let size = getSize board
        putStrLn "Where would you like to move?"
        putStrLn "(select a number on the board)"
        let (moves, numMap) = getMoves visited board chosen
        let newBoard = moves
        putStrLn (getDisplayString (placeUserMarker newBoard chosen) (size - 1))
        ans <- getLine
        let choice = fixDel ans
        if (all (\ l -> elem l nums) choice)
            then do
                let choiceInt = (read choice) :: Int
                if (choiceInt < 0 || choiceInt > length numMap)
                    then do
                        putStrLn "That is not a valid number. Restarting turn."
                        redoBoard <- doTurn visited board p
                        return redoBoard
                    else do
                        let newPos = numToCoords choiceInt numMap
                        let newBoard = doMove board chosen newPos
                        if (elem newBoard visited)
                            then do
                                putStrLn "This board has already existed. Choose another one."
                                redoBoard <- doTurn visited board p
                                return redoBoard
                            else do
                                nextBoard <- doTurn (board:visited) newBoard (getOtherPlayer p)
                                return nextBoard
            else do
                redoBoard <- doTurnMove visited board p chosen
                return redoBoard
        return board

doTurnStomp :: [Board] -> Board -> Piece -> Point -> IO Board
doTurnStomp visited board p chosen =
    do
        let size = getSize board
        putStrLn "Where would you like to stomp?"
        putStrLn "(select a number on the board)"
        let (stomps, numMap) = getStomps visited board p chosen
        let newBoard = stomps
        putStrLn (getDisplayString (placeUserMarker newBoard chosen) (size - 1))
        ans <- getLine
        let choice = fixDel ans
        if (all (\ l -> elem l nums) choice)
            then do
                let choiceInt = (read choice) :: Int
                if (choiceInt < 0 || choiceInt > length numMap)
                    then do
                        putStrLn "That is not a valid number. Restarting turn."
                        redoBoard <- doTurn visited board p
                        return redoBoard
                    else do
                        let newPos = numToCoords choiceInt numMap
                        let newBoard = doMove board chosen newPos
                        if (elem newBoard visited)
                            then do
                                putStrLn "This board has already existed. Choose another one."
                                redoBoard <- doTurn visited board p
                                return redoBoard
                            else do
                                nextBoard <- doTurn (board:visited) newBoard (getOtherPlayer p)
                                return nextBoard
            else do
                redoBoard <- doTurnMove visited board p chosen
                return redoBoard
        return board
