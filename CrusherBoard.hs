module CrusherBoard where

import Piece
import Util

data State = ContinueGame [Board] Board Piece

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
expandboard (NewBoard board) newSize size
    | size == newSize = expandboardhelper (NewBoard board) newSize
    | otherwise       = expandboard (expandboardhelper (NewBoard board) size) newSize (size + 1)

expandboardhelper :: Board -> Int -> Board
expandboardhelper (NewBoard board) newSize
    = NewBoard ([(replicate newSize W)] ++ (extendlines board) ++ [(replicate newSize B)])

extendlines :: [[Piece]] -> [[Piece]]
extendlines [] = []
extendlines (h:t) = ([X] ++ h ++ [X]):(extendlines t)

-- } /GENERATION FUNCTIONS

-- DISPLAY FUNCTIONS {

placeusermarker :: [[Piece]] -> Point -> [[Piece]]
placeusermarker board pos = setpos board pos P

placepiecemarkers :: [Board] -> [[Piece]] -> Piece -> ([[Piece]], [Point])
placepiecemarkers visited board p =
    (newboard, (getcoords board p (0, 0)))
    where newboard = placepiecemarkershelper visited board board p 1 (0, 0)

placepiecemarkershelper :: [Board] -> [[Piece]] -> [[Piece]] -> Piece -> Int -> Point -> [[Piece]]
placepiecemarkershelper _ _ [] _ _ _ = []
placepiecemarkershelper visited board (h:t) p n (y, 0) =
    (placepiecemarkersline visited board h p n (y, 0)):(placepiecemarkershelper visited board t p (n + (count h p)) (y + 1, 0))

placepiecemarkersline :: [Board] -> [[Piece]] -> [Piece] -> Piece -> Int -> Point -> [Piece]
placepiecemarkersline _ _ [] _ _ _ = []
placepiecemarkersline visited board (h:t) p n (y, x)
    | h == p && ((snd (getmoves visited board (y, x))) ++
                (snd (getstomps visited board p (y, x)))) /= [] =
        (Marker n):(placepiecemarkersline visited board t p (n + 1) (y, x + 1))
    | otherwise                                    = h:(placepiecemarkersline visited board t p n (y, x + 1))

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

domove :: [[Piece]] -> Point -> Point -> [[Piece]]
domove board old new = setpos (setpos board new (getpos board old)) old X

getmoves :: [Board] ->[[Piece]] -> Point -> ([[Piece]], [Point])
getmoves visited board pos = (getmovesloop board 1 moves, moves)
    where moves = getmovespaces visited board pos

getmovesloop :: [[Piece]] -> Int -> [Point] -> [[Piece]]
getmovesloop board _ [] = board
getmovesloop board n (h:t)
    | (getpos board h) == X = getmovesloop (setpos board h (Marker n)) (n + 1) t
    | otherwise             = getmovesloop board n t

getmovespaces :: [Board] -> [[Piece]] -> Point -> [Point]
getmovespaces visited board (y, x)
    | y == getsize board - 1 =
        validatespaces visited board ps (y, x)
            [(y - 1, x - 1), (y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x - 1), (y + 1, x)]
    | y < getsize board - 1 =
        validatespaces visited board ps (y, x)
            [(y - 1, x - 1), (y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x), (y + 1, x + 1)]
    | otherwise =
        validatespaces visited board ps (y, x)
            [(y - 1, x), (y - 1, x + 1), (y, x - 1), (y, x + 1), (y + 1, x - 1), (y + 1, x)]
    where ps = [W, B]

getstomps :: [Board] -> [[Piece]] -> Piece -> Point -> ([[Piece]], [Point])
getstomps visited board p pos = (getstompsloop board p 1 stomps, stomps)
    where stomps = getstompspaces visited board p pos

getstompsloop :: [[Piece]] -> Piece -> Int -> [Point] -> [[Piece]]
getstompsloop board _ _ [] = board
getstompsloop board p n (h:t)
    | elem (getpos board h) stompon = getstompsloop (setpos board h (Marker n)) p (n + 1) t
    | otherwise                     = getstompsloop board p n t
    where stompon = [(getotherplayer p), X]

getstompspaces :: [Board] -> [[Piece]] -> Piece -> Point -> [Point]
getstompspaces visited board p (y, x)
    | y < getsize board - 2 =
        validatespaces visited board ps (y, x)
            [(y - 2, x - 2), (y - 2, x), (y, x - 2), (y, x + 2), (y + 2, x), (y + 2, x + 2)]
    | y == getsize board - 2 =
        validatespaces visited board ps (y, x)
            [(y - 2, x - 2), (y - 2, x), (y, x - 2), (y, x + 2), (y + 2, x - 1), (y + 2, x + 1)]
    | y == getsize board - 1 =
        validatespaces visited board ps (y, x)
            [(y - 2, x - 2), (y - 2, x), (y, x - 2), (y, x + 2), (y + 2, x - 2), (y + 2, x)]
    | y == getsize board =
        validatespaces visited board ps (y, x)
            [(y - 2, x - 1), (y - 2, x + 1), (y, x - 2), (y, x + 2), (y + 2, x - 2), (y + 2, x)]
    | otherwise =
        validatespaces visited board ps (y, x)
            [(y - 2, x), (y - 2, x + 2), (y, x - 2), (y, x + 2), (y + 2, x - 2), (y + 2, x)]
    where ps = [p]

-- } /MOVEMENT FUNCTIONS

-- VALIDATION FUNCTIONS {

validatespaces :: [Board] -> [[Piece]] -> [Piece] -> Point -> [Point] -> [Point]
validatespaces _ _ _ _ [] = []
validatespaces visited board p old (h:t)
    | validspace visited board p h ([size..(2 * size - 2)] ++ [(2 * size - 1)] ++
          reverse [size..(2 * getsize board - 2)]) old
        = h:(validatespaces visited board p old t)
    | otherwise
        = validatespaces visited board p old t
    where size = getsize board

validspace :: [Board] -> [[Piece]] -> [Piece] -> Point -> [Int] -> Point -> Bool
validspace visited board p (y, x) sizes old =
    y >= 0 && y < length sizes && x >= 0 && x < sizes!!y && not (elem (board!!y!!x) p)
    && not (elem (NewBoard (domove board old (y, x))) visited)

-- } /VALIDATION FUNCTIONS

getsize :: [[Piece]] -> Int
getsize b = div (length b + 1) 2
