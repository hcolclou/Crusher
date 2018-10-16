module Crusher where

import Util
import CrusherBoard
import Piece
import AI

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

-- this constant can be changed to W or B when you want to play against AI
ai = [W, B]

play :: Brain -> Int -> IO Brain
play brain n =
    do
        putStrLn "Are you ready to play Crusher?"
        input <- getLine
        let ans = fixDel input
        if (elem ans yes)
            then do
                putStrLn rules
                turn <- doTurn brain [] (generate n) W
                return turn
            else do
                putStrLn "Oh, okay..."
                return brain

doTurn :: Brain -> [Board] -> Board -> Piece -> IO Brain
doTurn brain visited board p =
    do
        if (elem p ai)
            then do
                nextTurn <- (doAiTurn brain visited board p)
                return nextTurn
            else do
                nextTurn <- (doPlayerTurn brain visited board p)
                return nextTurn

doAiTurn :: Brain -> [Board] -> Board -> Piece -> IO Brain
doAiTurn brain visited board p =
    do
        putStrLn "Here is the current state:"
        putStrLn (getDisplayString board ((getSize board) - 1))
        let ps = countAll board p
        if (ps > 0)
            then do
                let newboard = choosePlay brain visited board p
                if (newboard == board)
                    then do
                        putStrLn "Game over"
                        finalBrain <- displayWinner brain visited p
                        return finalBrain
                    else do
                        putStrLn "Next turn"
                        nextBrain <- doTurn brain (board:visited) newboard (getOtherPlayer p)
                        return nextBrain
            else do
                finalBrain <- displayWinner brain (board:visited) p
                return finalBrain

displayWinner :: Brain -> [Board] -> Piece ->  IO Brain
displayWinner brain visited p =
    do
        putStrLn gameOver
        putStrLn ("The winner is " ++ (getDisplayChar (getOtherPlayer p)) ++ "!")
        newBrain <- remember brain visited p
        nextBrain <- play newBrain (getSize (visited !! 0))
        return nextBrain


doPlayerTurn :: Brain -> [Board] -> Board -> Piece -> IO Brain
doPlayerTurn brain visited board p =
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
                        newBrain <- doTurnMoveStomp brain visited board p ans
                        return newBrain
                    else do
                        putStrLn "That is not a number. Restarting turn."
                        newBrain <- doTurn brain visited board p
                        return newBrain
            else do
                finalBrain <- displayWinner brain (board:visited) p
                return finalBrain

doTurnMoveStomp :: Brain -> [Board] -> Board -> Piece -> Point -> IO Brain
doTurnMoveStomp brain visited board p chosen =
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
                        newBrain <- doTurnMove brain visited board p chosen
                        return newBrain
                    else do
                        newBrain <- doTurnStomp brain visited board p chosen
                        return newBrain
            else do
                putStrLn "That number is not valid. Restarting turn."
                newBrain <- doTurn brain visited board p
                return newBrain

doTurnMove :: Brain -> [Board] -> Board -> Piece -> Point -> IO Brain
doTurnMove brain visited board p chosen =
    do
        let size = getSize board
        putStrLn "Where would you like to move?"
        putStrLn "(select a number on the board)"
        let (moves, numMap) = getMoves visited board chosen
        let newBrain = moves
        putStrLn (getDisplayString (placeUserMarker newBrain chosen) (size - 1))
        ans <- getLine
        let choice = fixDel ans
        if (all (\ l -> elem l nums) choice)
            then do
                let choiceInt = (read choice) :: Int
                if (choiceInt < 0 || choiceInt > length numMap)
                    then do
                        putStrLn "That is not a valid number. Restarting turn."
                        redoBrain <- doTurn brain visited board p
                        return redoBrain
                    else do
                        let newPos = numToCoords choiceInt numMap
                        let newBrain = doMove board chosen newPos
                        if (elem newBrain visited)
                            then do
                                putStrLn "This board has already existed. Choose another one."
                                redoBrain <- doTurn brain visited board p
                                return redoBrain
                            else do
                                nextBrain <- doTurn brain (board:visited) newBrain (getOtherPlayer p)
                                return nextBrain
            else do
                redoBrain <- doTurnMove brain visited board p chosen
                return redoBrain

doTurnStomp :: Brain -> [Board] -> Board -> Piece -> Point -> IO Brain
doTurnStomp brain visited board p chosen =
    do
        let size = getSize board
        putStrLn "Where would you like to stomp?"
        putStrLn "(select a number on the board)"
        let (stomps, numMap) = getStomps visited board p chosen
        let newBrain = stomps
        putStrLn (getDisplayString (placeUserMarker newBrain chosen) (size - 1))
        ans <- getLine
        let choice = fixDel ans
        if (all (\ l -> elem l nums) choice)
            then do
                let choiceInt = (read choice) :: Int
                if (choiceInt < 0 || choiceInt > length numMap)
                    then do
                        putStrLn "That is not a valid number. Restarting turn."
                        redoBrain <- doTurn brain visited board p
                        return redoBrain
                    else do
                        let newPos = numToCoords choiceInt numMap
                        let newBrain = doMove board chosen newPos
                        if (elem newBrain visited)
                            then do
                                putStrLn "This board has already existed. Choose another one."
                                redoBrain <- doTurn brain visited board p
                                return redoBrain
                            else do
                                nextBrain <- doTurn brain (board:visited) newBrain (getOtherPlayer p)
                                return nextBrain
            else do
                redoBrain <- doTurnMove brain visited board p chosen
                return redoBrain
