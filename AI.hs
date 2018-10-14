module AI where

import CrusherBoard
import Piece
import Util
import Control.Parallel

depth = 2
parallel = True

calculatevalue :: [[Piece]] -> Piece -> Double
calculatevalue board me
    | x == 0    = 0
    | sign > 0  = 1.5*(sqrt (x / (1.0 + x)))
    | otherwise = (-1.0)*(sqrt (x / (1.0 + x)))
    where x = fromIntegral (abs ((countall board me) - (countall board (getotherplayer me))))
          sign = (x / (sqrt (x * x)))

calculateoddssingle :: [Board] -> Board -> Piece -> Piece -> Int -> Double
calculateoddssingle visited (NewBoard board) me curr depth
    | countall board curr == 0 = if curr == me then -1.0 else 1.5
    | plays == [] = if curr == me then -1.0 else 1.5
    | depth == 0 = calculatevalue board me
    | otherwise = (calculateoddsmult ((NewBoard board):visited) plays me (getotherplayer curr) depth) / (fromIntegral (length plays))
    where plays = generateplayset visited (NewBoard board) curr

calculateoddsmult :: [Board] -> [Board] -> Piece -> Piece -> Int -> Double
calculateoddsmult _ [] _ _ _ = 0.0
calculateoddsmult visited (h:t) me curr depth
    | parallel  = par p1 (pseq p2 (p1 + p2))
    | otherwise = calculateoddssingle visited h me curr (depth - 1) + calculateoddsmult visited t me curr depth
    where p1 = calculateoddssingle visited h me curr (depth - 1)
          p2 = calculateoddsmult visited t me curr depth

generateplayset :: [Board] -> Board -> Piece -> [Board]
generateplayset visited (NewBoard board) p = generateplaysethelp visited board pieces p
    where pieces = snd (placepiecemarkers visited board p)

generateplaysethelp :: [Board] -> [[Piece]] -> [Point] -> Piece -> [Board]
generateplaysethelp _ _ [] _ = []
generateplaysethelp visited board (h:t) p = (generatemoves board h (moves ++ stomps)) ++ (generateplaysethelp visited board t p)
    where moves = snd (getmoves visited board h)
          stomps = snd (getstomps visited board p h)

generatemoves :: [[Piece]] -> Point -> [Point] -> [Board]
generatemoves _ _ [] = []
generatemoves board old (new:t) = (NewBoard (domove board old new)):(generatemoves board old t)

chooseplay :: [Board] -> Board -> Piece -> Board
chooseplay visited board p
    | (length plays) == 0 = board
    | otherwise = find plays (maximum (getprobs visited plays p)) (getprobs visited plays p)
    where plays = generateplayset visited board p

getprobs :: [Board] -> [Board] -> Piece -> [Double]
getprobs _ [] _ = []
getprobs visited (h:t) p = (calculateoddssingle visited h p p depth):(getprobs visited t p)

find :: [Board] -> Double -> [Double] -> Board
find (h1:t1) m (h2:t2)
    | h2 == m    = h1
    | otherwise  = find t1 m t2
