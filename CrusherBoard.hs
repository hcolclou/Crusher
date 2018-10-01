module CrusherBoard where

import Piece

-- GENERATION FUNCTIONS {

generate :: Int -> [[Piece]]
generate n
    | n > 1     = expandboard baseState n 2
    | otherwise = baseState

expandboard :: [[Piece]] -> Int -> Int -> [[Piece]]
expandboard board newSize size
    | size == newSize = expandboardhelper board newSize
    | otherwise       = expandboard (expandboardhelper board size) newSize (size + 1)

expandboardhelper :: [[Piece]] -> Int -> [[Piece]]
expandboardhelper board newSize
    = [(replicate newSize W)] ++ (extendlines board) ++ [(replicate newSize B)]

extendlines :: [[Piece]] -> [[Piece]]
extendlines [] = []
extendlines (h:t) = ([X] ++ h ++ [X]):(extendlines t)

-- } /GENERATION FUNCTIONS

-- DISPLAY FUNCTIONS {

placeuser :: [[Piece]] -> Int -> Piece -> [[Piece]]
placeuser (h:t) n p
    | x < n     = h:(placeuser t (n - x) p)
    | otherwise = (placeuserhelper h n p):t
    where x = count h p

placeuserhelper :: [Piece] -> Int -> Piece -> [Piece]
placeuserhelper (h:t) n p
    | h == p && n == 1 = P:t
    | h == p           = h:(placeuserhelper t (n - 1) p)
    | otherwise        = h:(placeuserhelper t n p)

placepiecemarkers :: [[Piece]] -> Piece -> Int -> [[Piece]]
placepiecemarkers [] _ _ = []
placepiecemarkers (h:t) p n = (replace h p n):(placepiecemarkers t p (n + (count h p)))

replace :: [Piece] -> Piece -> Int -> [Piece]
replace [] _ _ = []
replace (h:t) p n
    | h == p    = (Marker n):(replace t p (n + 1))
    | otherwise = h:(replace t p n)

getdisplaystring :: [[Piece]] -> Int -> String
getdisplaystring [] _ = ""
getdisplaystring (h:t) n
    = (replicate (abs n) ' ') ++ (getdisplaystringline h) ++ "\n" ++ (getdisplaystring t (n - 1))

getdisplaystringline :: [Piece] -> String
getdisplaystringline []    = ""
getdisplaystringline (h:t) = (getdisplaychar h) ++ " " ++(getdisplaystringline t)

getdisplaychar :: Piece -> String
getdisplaychar X = "-"
getdisplaychar W = "W"
getdisplaychar B = "B"
getdisplaychar P = "P"
getdisplaychar (Marker n) = show n

-- } /DISPLAY FUNCTIONS

-- MOVEMENT FUNCTIONS {

getmovespaces :: (Int, Int) -> Int -> [(Int, Int)]
getmovespaces (y, x) size
    | y == size - 1
        = validatespaces [(y - 1, x - 1), (y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x - 1), (y + 1, x)] size
    | y < size - 1
        = validatespaces [(y - 1, x - 1), (y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x), (y + 1, x + 1)] size
    | otherwise
        = validatespaces [(y - 1, x), (y - 1, x + 1), (y, x - 1), (y, x + 1), (y + 1, x - 1), (y + 1, x)] size

-- } /MOVEMENT FUNCTIONS

-- VALIDATION FUNCTIONS {

validatespaces :: [(Int, Int)] -> Int -> [(Int, Int)]
validatespaces [] _ = []
validatespaces (h:t) size
    | validspace h ([size..(2 * size - 2)] ++ [(2 * size - 1)] ++ reverse [size..(2 * size - 2)])
        = h:(validatespaces t size)
    | otherwise
        = validatespaces t size

validspace :: (Int, Int) -> [Int] -> Bool
validspace (y, x) sizes = y >= 0 && y < length sizes && x >= 0 && x < sizes!!y

-- } /VALIDATION FUNCTIONS
