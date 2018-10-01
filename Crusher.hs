module Crusher where

data Piece = W | B | X | P | Marker Int
    deriving (Eq)

boardsize = 4
baseState = [[X]]
yes   = ["y", "yes", "ye"]
move  = ["m", "mo", "mov", "move"]
nums  = "0123456789"
rules = "Rules: \n"

play :: IO [[Piece]]
play =
    do
        putStrLn "Are you ready to play?"
        input <- getLine
        let ans = fixdel input
        if (elem ans yes)
            then do
                putStrLn rules
                doturn (generate boardsize) W
            else do
                putStrLn "Oh, okay..."
                return baseState

doturn :: [[Piece]] -> Piece -> IO [[Piece]]
doturn board p =
    do
        putStrLn ("It is " ++ (getdisplaychar p) ++ "'s turn!")
        putStrLn "Type 'x' at any time to restart the turn."
        putStrLn "Which piece would you like to move?"
        putStrLn "(select a number on the board)"
        putStrLn (getdisplaystring (placepiecemarkers board p 1) (boardsize - 1))
        input <- getLine
        let chosenpiece = fixdel input
        if (chosenpiece == "x")
            then do
                newboard <- doturn board p
                return newboard
            else do
                if (all (\ l -> elem l nums) chosenpiece)
                    then do
                        let ans = (read chosenpiece) :: Int
                        doturnmovestomp board p ans
                    else do
                        putStrLn "That is not a number. Restarting turn."
                        newboard <- doturn board p
                        return newboard

doturnmovestomp :: [[Piece]] -> Piece -> Int -> IO [[Piece]]
doturnmovestomp board p ans =
    do
        if (ans > 0 && ans <= (countall board p))
            then do
                putStrLn "You have selected the piece called 'P':"
                putStrLn (getdisplaystring (placeuser board ans p) (boardsize - 1))
                putStrLn "Would you like to move or stomp?"
                input <- getLine
                let ans = fixdel input
                if (ans == "x")
                    then do
                        putStrLn "Restarting turn."
                        newboard <- doturn board p
                        return newboard
                    else do
                        if (elem ans move)
                            then do
                                putStrLn "Where would you like to move?"
                                putStrLn "(select a number on the board)"
                                putStrLn "This is the end of the implementation so far..."
                                return board
                            else do
                                putStrLn "Where would you like to stomp?"
                                putStrLn "(select a number on the board)"
                                putStrLn "This is the end of the implementation so far..."
                                return board
            else do
                putStrLn "That number is not valid. Restarting turn."
                newboard <- doturn board p
                return newboard

getotherplayer :: Piece -> Piece
getotherplayer W = B
getotherplayer B = W

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

countall :: [[Piece]] -> Piece -> Int
countall [] _ = 0
countall (h:t) p = (count h p) + (countall t p)

count :: [Piece] -> Piece -> Int
count [] _ = 0
count (h:t) p
    | h == p    = 1 + count t p
    | otherwise = count t p

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

getmovespaces :: (Int, Int) -> Int -> [(Int, Int)]
getmovespaces (y, x) size
    | y == size - 1
        = validatespaces [(y - 1, x - 1), (y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x - 1), (y + 1, x)] size
    | y < size - 1
        = validatespaces [(y - 1, x - 1), (y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x), (y + 1, x + 1)] size
    | otherwise
        = validatespaces [(y - 1, x), (y - 1, x + 1), (y, x - 1), (y, x + 1), (y + 1, x - 1), (y + 1, x)] size

validatespaces :: [(Int, Int)] -> Int -> [(Int, Int)]
validatespaces [] _ = []
validatespaces (h:t) size
    | validspace h ([size..(2 * size - 2)] ++ [(2 * size - 1)] ++ reverse [size..(2 * size - 2)])
        = h:(validatespaces t size)
    | otherwise
        = validatespaces t size

validspace :: (Int, Int) -> [Int] -> Bool
validspace (y, x) sizes = y >= 0 && y < length sizes && x >= 0 && x < sizes!!y

go = play

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

{-
(2, 2) -> (1, 1)         | (1, 2)     | (2, 1)     | (2, 3)     | (3, 1)         | (3, 2)
(y, x) -> (y - 1, x - 1) | (y - 1, x) | (y, x - 1) | (y, x + 1) | (y + 1, x - 1) | (y + 1, x)

(1, 1) -> (0, 0) | (0, 1) | (1, 0) | (1, 2) | (2, 1) | (2, 2)

(4, 1) -> (3, 1) | (3, 2) | (4, 0) | (4, 2) | (5, 0) | (5, 1)
-}
