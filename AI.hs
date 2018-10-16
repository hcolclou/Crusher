module AI (Brain, remember, choosePlay, aiTests) where

import Test.HUnit
import CrusherBoard
import Piece
import Util
import Control.Parallel
import Data.Hashable
import Data.Map as Map

depth     = 1
parallel  = True

type Brain = Map Int Int

remember :: Brain -> [Board] -> Piece -> IO Brain
remember brain [] _ = return brain
remember brain (h:t) winner =
    do
        if (member boardHash brain)
            then do
                if (winner == W)
                    then do
                        let newBrain = insert boardHash ((brain ! boardHash) + 1) brain
                        newBrain <- (remember newBrain t winner)
                        return newBrain
                    else do
                        let newBrain = insert boardHash ((brain ! boardHash) - 1) brain
                        newBrain <- (remember newBrain t winner)
                        return newBrain
            else do
                if (winner == W)
                    then do
                        let newBrain = insert boardHash 1 brain
                        newBrain <- (remember newBrain t winner)
                        return newBrain
                    else do
                        let newBrain = insert boardHash (-1) brain
                        newBrain <- (remember newBrain t winner)
                        return newBrain
    where boardHash = hash (getDisplayString h (getSize h - 1))

{-
calculateValue is a function that takes a Board and a Piece and evaluates the
board from that Piece's side, returning a Double between negMult and posMult
that represents how good the situation is.
@param board The board to calculate a value for.
@param me    The piece to evaluate the board for.
-}
calculateValue :: Brain -> Board -> Piece -> Double
calculateValue brain board me
    | member boardHash brain && me == W =
        fromIntegral (currSum + (brain ! boardHash))
    | member boardHash brain && me == B =
        fromIntegral (currSum - (brain ! boardHash))
    | otherwise = fromIntegral currSum
    where boardHash = hash (getDisplayString board (getSize board - 1))
          currSum = (countAll board me) - (countAll board (getOtherPlayer me))

{-
calculateOddsSingle takes a list of Boards, a Board, two Pieces, and an Int and
calculates the value of this state by taking the average of each value from each
of the possible moves from this state.
@param visited The list of boards that have already occured.
@param board   The board to calculate a value for.
@param me      The piece type of the AI.
@param curr    The piece type of the current player.
@param depth   The depth that the calculations have yet to go.
-}
calculateOddsSingle :: Brain -> [Board] -> Board -> Piece -> Piece -> Int -> Double
calculateOddsSingle brain visited board me curr depth
    | countAll board curr == 0 || plays == [] || depth == 0 =
        (fromIntegral depth + 1.0) * (calculateValue brain board me)
    | otherwise =
        (calculateOddsMult brain (board:visited) plays me (getOtherPlayer curr) depth)
    where plays = generatePlayset visited board curr

{-
calculateOddsMult calculates the average value for each Board in its second list
of Boards.
@param visited The list of boards that have already occured.
@param (h:t)   The list of boards to calculate values for.
@param me      The piece type of the AI.
@param curr    The piece type of the current player.
@param depth   The depth that the calculations have yet to go.
-}
calculateOddsMult :: Brain -> [Board] -> [Board] -> Piece -> Piece -> Int -> Double
calculateOddsMult _ _ [] _ _ _ = 0.0
calculateOddsMult brain visited (h:t) me curr depth
    | parallel  = par p1 (pseq p2 (p1 + p2))
    | otherwise = p1 + p2
    where p1 = calculateOddsSingle brain visited h me curr (depth - 1)
          p2 = calculateOddsMult brain visited t me curr depth

{-
generatePlayset takes a list of Boards, a Board, and a Piece and generates a
list of all the possible moves that the player with type Piece can make.
@param visited The list of boards that have already occured.
@param board   The current board.
@param p       The piece type to generate moves for.
-}
generatePlayset :: [Board] -> Board -> Piece -> [Board]
generatePlayset visited board p = generatePlaysetHelp visited board pieces p
    where pieces = snd (placePieceMarkers visited board p)

{-
generatePlaysetHelp takes a list of Boards, a Board, a list of Points, and a
Piece and generates all the possible moves for a Piece at each Point.
@param visited The list of boards that have already occured.
@param board   The current board.
@param (h:t)   The list of coordinates of pieces to generate moves for.
@param p       The piece type to generate moves for.
-}
generatePlaysetHelp :: [Board] -> Board -> [Point] -> Piece -> [Board]
generatePlaysetHelp _ _ [] _ = []
generatePlaysetHelp visited board (h:t) p = single ++ rest
    where moves = snd (getMoves visited board h)
          stomps = snd (getStomps visited board p h)
          single = generateMoves board h (moves ++ stomps)
          rest = generatePlaysetHelp visited board t p

{-
generateMoves takes a Board, a Point, and List of Points and it generates a list
of all the boards that can be made from moving the first Point to the Points ins
the list.
@param board   The current board.
@param old     The old position of a piece.
@param (new:t) The list of coordinates to get moves to.
-}
generateMoves :: Board -> Point -> [Point] -> [Board]
generateMoves _ _ [] = []
generateMoves board old (new:t) = (doMove board old new):(generateMoves board old t)

{-
choosePlay is the main function in AI, it takes a list of Boards, a Board, a
Piece, and returns a new Board which is the move that it calculated to have the
best odds for the given Piece to win.
@param visited The list of boards that have already occured.
@param board   The current board.
@param p       The AI's piece type.
-}
choosePlay :: Brain -> [Board] -> Board -> Piece -> Board
choosePlay brain visited board p
    | (length plays) == 0 = board
    | otherwise = find plays (maximum probs) probs
    where plays = generatePlayset visited board p
          probs = getProbs brain visited plays p

{-
getProbs takes two lists of Boards and a Piece and gets a list of all the odds
associated with each possible move it can make.
@param visited The list of baords that have already occured.
@param (h:t)   The list of boards to calculate odds for.
@param p       The AI's piece type.
-}
getProbs :: Brain -> [Board] -> [Board] -> Piece -> [Double]
getProbs _ _ [] _ = []
getProbs brain visited (h:t) p = single:rest
    where single = calculateOddsSingle brain visited h p p depth
          rest = getProbs brain visited t p

{-
find takes a list of Boards, a Double, and a list of Doubles and returns the
Board associated with the given Double.
@param (h1:t1) The list of boards to go through.
@param m       The maximum value to search for.
@param (h2:t2) The list of odds that correspond to the boards.
-}
find :: [Board] -> Double -> [Double] -> Board
find (h1:t1) m (h2:t2)
    | h2 == m    = h1
    | otherwise  = find t1 m t2

-- AI Tests
aiTests = TestList []
