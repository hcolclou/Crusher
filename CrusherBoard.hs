module CrusherBoard where

import Piece
import Util

data Board = NewBoard [[Piece]]
    deriving (Eq)

baseState = NewBoard [[X]]

-- TODO: write purposes for functions

-- GENERATION FUNCTIONS {

generate :: Int -> Board
generate n
    | n > 1     = expandboard baseState n 2
    | otherwise = baseState

expandboard :: Board -> Int -> Int -> Board
expandboard board newSize size
    | size == newSize = expandboardhelper board newSize
    | otherwise       = expandboard (expandboardhelper board size) newSize (size + 1)

expandboardhelper :: Board -> Int -> Board
expandboardhelper (NewBoard board) newSize
    = NewBoard ([(replicate newSize W)] ++ (extendlines board) ++ [(replicate newSize B)])

extendlines :: [[Piece]] -> [[Piece]]
extendlines [] = []
extendlines (h:t) = ([X] ++ h ++ [X]):(extendlines t)

-- } /GENERATION FUNCTIONS

-- DISPLAY FUNCTIONS {

placeusermarker :: [[Piece]] -> (Int, Int) -> [[Piece]]
placeusermarker board pos = setpos board pos P

placepiecemarkers :: [[Piece]] -> Piece -> ([[Piece]], [(Int, Int)])
placepiecemarkers board p =
    (newboard, (getcoords board p (0, 0)))
    where newboard = placepiecemarkershelper board p 1

placepiecemarkershelper :: [[Piece]] -> Piece -> Int -> [[Piece]]
placepiecemarkershelper [] _ _ = []
placepiecemarkershelper (h:t) p n =
    (placepiecemarkersline h p n):(placepiecemarkershelper t p (n + (count h p)))

placepiecemarkersline :: [Piece] -> Piece -> Int -> [Piece]
placepiecemarkersline [] _ _ = []
placepiecemarkersline (h:t) p n
    | h == p    = (Marker n):(placepiecemarkersline t p (n + 1))
    | otherwise = h:(placepiecemarkersline t p n)

getdisplaystring :: [[Piece]] -> Int -> String
getdisplaystring [] _ = ""
getdisplaystring (h:t) n =
    (replicate (abs n) ' ') ++ (getdisplaystringline h) ++ "\n" ++ (getdisplaystring t (n - 1))

getdisplaystringline :: [Piece] -> String
getdisplaystringline []    = ""
getdisplaystringline (h:t) = (getdisplaychar h) ++ " " ++ (getdisplaystringline t)

getdisplaychar :: Piece -> String
getdisplaychar X = "-"
getdisplaychar W = "W"
getdisplaychar B = "B"
getdisplaychar P = "P"
getdisplaychar (Marker n) = show n

-- } /DISPLAY FUNCTIONS

-- MOVEMENT FUNCTIONS {

domove :: [[Piece]] -> (Int, Int) -> (Int, Int) -> [[Piece]]
domove board old new = setpos (setpos board new (getpos board old)) old X

getmoves :: [[Piece]] -> Int -> (Int, Int) -> ([[Piece]], [(Int, Int)])
getmoves board size pos = (getmovesloop board 1 moves, moves)
    where moves = getmovespaces board pos size

getmovesloop :: [[Piece]] -> Int -> [(Int, Int)] -> [[Piece]]
getmovesloop board _ [] = board
getmovesloop board n (h:t)
    | (getpos board h) == X = getmovesloop (setpos board h (Marker n)) (n + 1) t
    | otherwise             = getmovesloop board n t

getmovespaces :: [[Piece]] -> (Int, Int) -> Int -> [(Int, Int)]
getmovespaces board (y, x) size
    | y == size - 1 =
        validatespaces board ps size
            [(y - 1, x - 1), (y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x - 1), (y + 1, x)]
    | y < size - 1 =
        validatespaces board ps size
            [(y - 1, x - 1), (y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x), (y + 1, x + 1)]
    | otherwise =
        validatespaces board ps size
            [(y - 1, x), (y - 1, x + 1), (y, x - 1), (y, x + 1), (y + 1, x - 1), (y + 1, x)]
    where ps = [W, B]

getstomps :: [[Piece]] -> Piece -> Int -> (Int, Int) -> ([[Piece]], [(Int, Int)])
getstomps board p size pos = (getstompsloop board p 1 stomps, stomps)
    where stomps = getstompspaces board p pos size

getstompsloop :: [[Piece]] -> Piece -> Int -> [(Int, Int)] -> [[Piece]]
getstompsloop board _ _ [] = board
getstompsloop board p n (h:t)
    | elem (getpos board h) stompon = getstompsloop (setpos board h (Marker n)) p (n + 1) t
    | otherwise                     = getstompsloop board p n t
    where stompon = [(getotherplayer p), X]

getstompspaces :: [[Piece]] -> Piece -> (Int, Int) -> Int -> [(Int, Int)]
getstompspaces board p (y, x) size
    | y < size - 2 =
        validatespaces board ps size
            [(y - 2, x - 2), (y - 2, x), (y, x - 2), (y, x + 2), (y + 2, x), (y + 2, x + 2)]
    | y == size - 2 =
        validatespaces board ps size
            [(y - 2, x - 2), (y - 2, x), (y, x - 2), (y, x + 2), (y + 2, x - 1), (y + 2, x + 1)]
    | y == size - 1 =
        validatespaces board ps size
            [(y - 2, x - 2), (y - 2, x), (y, x - 2), (y, x + 2), (y + 2, x - 2), (y + 2, x)]
    | y == size =
        validatespaces board ps size
            [(y - 2, x - 1), (y - 2, x + 1), (y, x - 2), (y, x + 2), (y + 2, x - 2), (y + 2, x)]
    | otherwise =
        validatespaces board ps size
            [(y - 2, x), (y - 2, x + 2), (y, x - 2), (y, x + 2), (y + 2, x - 2), (y + 2, x)]
    where ps = [p]

-- } /MOVEMENT FUNCTIONS

-- VALIDATION FUNCTIONS {

validatespaces :: [[Piece]] -> [Piece] -> Int -> [(Int, Int)] -> [(Int, Int)]
validatespaces _ _ _ [] = []
validatespaces board p size (h:t)
    | validspace board p h ([size..(2 * size - 2)] ++ [(2 * size - 1)] ++
          reverse [size..(2 * size - 2)])
        = h:(validatespaces board p size t)
    | otherwise
        = validatespaces board p size t

validspace :: [[Piece]] -> [Piece] -> (Int, Int) -> [Int] -> Bool
validspace board p (y, x) sizes =
    y >= 0 && y < length sizes && x >= 0 && x < sizes!!y && not (elem (board!!y!!x) p)

-- } /VALIDATION FUNCTIONS


{-
    | y < size - 2
(y, x) -> (y - 2, x - 2), (y - 2, x), (y, x - 2), (y, x + 2), (y + 2, x), (y + 2, x + 2)

    | y == size - 2
(y, x) -> (y - 2, x - 2), (y - 2, x), (y, x - 2), (y, x + 2), (y + 2, x - 1), (y + 2, x + 1)

    | y == size - 1
(y, x) -> (y - 2, x - 2), (y - 2, x), (y, x - 2), (y, x + 2), (y + 2, x - 2), (y + 2, x)

    | y == size
(y, x) -> (y - 2, x - 1), (y - 2, x + 1), (y, x - 2), (y, x + 2), (y + 2, x - 2), (y + 2, x)

    | y > size
(y, x) -> (y - 2, x), (y - 2, x + 2), (y, x - 2), (y, x + 2), (y + 2, x - 2), (y + 2, x)

-}
