module AI where

import CrusherBoard
import Piece
import Util

-- this function will calculate the odds of winning given the board a colour to play
calculateodds :: [Board] -> Board -> Piece -> (Int, Int)
calculateodds visited board p = (1, 1)

-- this function will generate all possible plays given a state and a colour to play
generateplayset :: [Board] -> Board -> Piece -> [Board]
generateplayset visited board p = [board] -- stub

-- this function will use odds calculations to choose the best play
chooseplay :: [Board] -> Board -> Piece -> Board
chooseplay visited board p = board

-- more functions... try to use the ones in CrusherBoard.hs, Piece.hs, and Util.hs
-- before writing new ones
