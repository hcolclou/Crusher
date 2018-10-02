module Util where

import Piece

-- TODO: write purposes for functions

{-
A constant for the backspace symbol.
-}
delchar :: String
delchar = "\DEL"

{-
A wrapper for a function which handles deletions.
-}
fixdel :: String -> String
fixdel str = fixdelper [] str

{-
Removes all delete characters and the characters that they were meant to delete.
-}
fixdelper :: String -> String -> String
fixdelper []  str
    | prefix delchar str    = fixdelper [] (removestr (length delchar) str)
    | contains delchar str  = fixdelper [head str] (tail str)
    | otherwise             = str
fixdelper acc str
    | prefix delchar str    = fixdelper (init acc) (removestr (length delchar) str)
    | contains delchar str  = fixdelper (acc ++ [head str]) (tail str)
    | otherwise             = acc ++ str


{-
Returns true if the second string contains the first string.
-}
contains :: String -> String -> Bool
contains (h:t) []           = False
contains s1    s2
    | prefix s1 s2          = True
    | contains s1 (tail s2) = True
    | otherwise             = False

{-
Returns true if the first part of the second string is equal to the first
string.
-}
prefix :: String -> String -> Bool
prefix []      _       = True
prefix (h:t)   []      = False
prefix (h1:t1) (h2:t2) = (h1 == h2) && prefix t1 t2

{-
Removes the first n characters of a string.
-}
removestr :: Int -> String -> String
removestr 0 str     = str
removestr n []      = []
removestr n (h2:t2) = removestr (n - 1) t2

numtocoords :: Int -> [(Int, Int)] -> (Int, Int)
numtocoords n list
    | n > length list = (-1, -1)
    | otherwise       = list!!(n - 1)

getcoords :: [[Piece]] -> Piece -> (Int, Int) -> [(Int, Int)]
getcoords [] _ _ = []
getcoords (h:t) p (y, _) = (getcoordshelper h p (y, 0)) ++ (getcoords t p (y + 1, 0))

getcoordshelper :: [Piece] -> Piece -> (Int, Int) -> [(Int, Int)]
getcoordshelper [] _ _ = []
getcoordshelper (h:t) p (y, x)
    | h == p    = (y, x):(getcoordshelper t p (y, x + 1))
    | otherwise = getcoordshelper t p (y, x + 1)

getpos :: [[Piece]] -> (Int, Int) -> Piece
getpos (h:t) (y, x)
    | y == 0    = getposhelper h x
    | otherwise = getpos t (y - 1, x)

getposhelper :: [Piece] -> Int -> Piece
getposhelper (h:t) x
    | x == 0    = h
    | otherwise = getposhelper t (x - 1)

setpos :: [[Piece]] -> (Int, Int) -> Piece -> [[Piece]]
setpos (h:t) (y, x) p
    | y == 0    = (setposhelper h x p):t
    | otherwise = h:(setpos t (y - 1, x) p)

setposhelper :: [Piece] -> Int -> Piece -> [Piece]
setposhelper (h:t) x p
    | x == 0    = p:t
    | otherwise = h:(setposhelper t (x - 1) p)
