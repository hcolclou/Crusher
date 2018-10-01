module Util where

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
