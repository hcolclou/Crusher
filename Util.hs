module Util (fixDel, numToCoords, getCoords, getPos, setPos, utilTests) where

import Test.HUnit
import Piece

{-
A constant for the backspace symbol.
-}
delChar :: String
delChar = "\DEL"

{-
A wrapper for a function which handles deletions.
-}
fixDel :: String -> String
fixDel str = fixDelper [] str

{-
Removes all delete characters and the characters that they were meant to delete.
-}
fixDelper :: String -> String -> String
fixDelper []  str
    | prefix delChar str    = fixDelper [] (removeStr (length delChar) str)
    | contains delChar str  = fixDelper [head str] (tail str)
    | otherwise             = str
fixDelper acc str
    | prefix delChar str    = fixDelper (init acc) (removeStr (length delChar) str)
    | contains delChar str  = fixDelper (acc ++ [head str]) (tail str)
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
removeStr :: Int -> String -> String
removeStr 0 str     = str
removeStr n []      = []
removeStr n (h2:t2) = removeStr (n - 1) t2

{-
Takes an int and a list of Points and returns the point at position n where n
is 1-based.
-}
numToCoords :: Int -> [Point] -> Point
numToCoords _ [] = (-1, -1)
numToCoords n list
    | n > length list = (-1, -1)
    | otherwise       = list!!(n - 1)

{-
Returns a list of Points that describe all the locations of Piece p.
-}
getCoords :: Board -> Piece -> Point -> [Point]
getCoords [] _ _ = []
getCoords (h:t) p (y, _) = (getCoordsHelper h p (y, 0)) ++ (getCoords t p (y + 1, 0))

{-
Helper function for getCoords that gets the coords when it's already on the
right row.
-}
getCoordsHelper :: Row -> Piece -> Point -> [Point]
getCoordsHelper [] _ _ = []
getCoordsHelper (h:t) p (y, x)
    | h == p    = (y, x):(getCoordsHelper t p (y, x + 1))
    | otherwise = getCoordsHelper t p (y, x + 1)

{-
getPos takes a Board and a Point and returns the Piece type at that Point.
-}
getPos :: Board -> Point -> Piece
getPos [] p = P
getPos (h:t) (y, x)
    | y == 0    = getPosHelper h x
    | otherwise = getPos t (y - 1, x)

{-
getPosHelper is a helper function for getPos that takes a Row and a Point and
returns the Piece at that Point.
-}
getPosHelper :: Row -> Int -> Piece
getPosHelper [] x = P
getPosHelper (h:t) x
    | x == 0    = h
    | otherwise = getPosHelper t (x - 1)

{-
setPos takes a Board, a Point, and a Piece and returns a new Board with the
Piece at Point changed to the given Piece.
-}
setPos :: Board -> Point -> Piece -> Board
setPos [] x p = []
setPos (h:t) (y, x) p
    | y == 0    = (setPosHelper h x p):t
    | otherwise = h:(setPos t (y - 1, x) p)

{-
setPosHelper is a helper function for setPos that takes a Row, an Int, and a
Piece and returns a new Row with the Piece at the given Int changed to the
given Piece.
-}
setPosHelper :: Row -> Int -> Piece -> Row
setPosHelper [] x p = []
setPosHelper (h:t) x p
    | x == 0    = p:t
    | otherwise = h:(setPosHelper t (x - 1) p)




-- Util Tests
utilTests = TestList [fdTests, fdhTests, conTests, pfTests, rsTests,
    ntcTests, gcTests, gchTests, gpTests, gphTests, spTests, sphTests]

fdTests = TestList [
    TestLabel "fixDel Test 1" fdTestEmpty,
    TestLabel "fixDel Test 2" fdTestDel1,
    TestLabel "fixDel Test 3" fdTest1Remain,
    TestLabel "fixDel Test 4" fdTestDelBoth,
    TestLabel "fixDel Test 5" fdTestDelNone
    ]
fdTestEmpty   = TestCase (assertEqual "for (fixDel \"\")," "" (fixDel ""))
fdTestDel1    = TestCase (assertEqual "for (fixDel \"\\DEL\")," "" (fixDel "\DEL"))
fdTest1Remain = TestCase (assertEqual "for (fixDel \"a\\DELb\")," "b" (fixDel "a\DELb"))
fdTestDelBoth = TestCase (assertEqual "for (fixDel \"a\\DELb\\DEL\")," "" (fixDel "a\DELb\DEL"))
fdTestDelNone = TestCase (assertEqual "for (fixDel \"ab\")," "ab" (fixDel "ab"))

fdhTests = TestList [
    TestLabel "fixDelper Test 1" fdhTestEmpty,
    TestLabel "fixDelper Test 2" fdhTestRemain,
    TestLabel "fixDelper Test 3" fdhTestDel1,
    TestLabel "fixDelper Test 4" fdhTestDelBoth,
    TestLabel "fixDelper Test 5" fdhTestDelNone,
    TestLabel "fixDelper Test 6" fdhTest1Alread,
    TestLabel "fixDelper Test 7" fdhTest2Alread,
    TestLabel "fixDelper Test 8" fdhTest2Alrnd
    ]
fdhTestEmpty   = TestCase (assertEqual "for (fixDelper \"\" \"\")," "" (fixDelper "" ""))
fdhTestRemain  = TestCase (assertEqual "for (fixDelper \"\" \"a\\DELb\")," "b" (fixDelper "" "a\DELb"))
fdhTestDel1    = TestCase (assertEqual "for (fixDelper \"\" \"\\DEL\")," "" (fixDelper "" "\DEL"))
fdhTestDelBoth = TestCase (assertEqual "for (fixDelper \"\" \"a\\DELb\\DEL\")," "" (fixDelper "" "a\DELb\DEL"))
fdhTestDelNone = TestCase (assertEqual "for (fixDelper \"\" \"ab\")," "ab" (fixDelper "" "ab"))
fdhTest1Alread = TestCase (assertEqual "for (fixDelper \"a\" \"\\DEL\")," "" (fixDelper "a" "\DEL"))
fdhTest2Alread = TestCase (assertEqual "for (fixDelper \"ab\" \"\\DEL\")," "a" (fixDelper "ab" "\DEL"))
fdhTest2Alrnd  = TestCase (assertEqual "for (fixDelper \"ab\" \"c\\DELd\")," "abd" (fixDelper "ab" "c\DELd"))

conTests = TestList [
    TestLabel "contains Test 1" conTestEmpty,
    TestLabel "contains Test 2" conTestBad,
    TestLabel "contains Test 3" conTestGood,
    TestLabel "contains Test 4" conTestTrue1,
    TestLabel "contains Test 5" conTestTrue2,
    TestLabel "contains Test 6" conTestTrue3,
    TestLabel "contains Test 7" conTestFalse1,
    TestLabel "contains Test 8" conTestFalse2,
    TestLabel "contains Test 9" conTestFalse3
    ]
conTestEmpty  = TestCase (assertEqual "for (contains [] [])," True (contains [] []))
conTestBad    = TestCase (assertEqual "for (contains \"a\" [])," False (contains "a" []))
conTestGood   = TestCase (assertEqual "for (contains [] \"a\")," True (contains [] "a"))
conTestTrue1  = TestCase (assertEqual "for (contains \"a\" \"a\")," True (contains "a" "a"))
conTestTrue2  = TestCase (assertEqual "for (contains \"b\" \"abc\")," True (contains "b" "abc"))
conTestTrue3  = TestCase (assertEqual "for (contains \"bcd\" \"abcd\")," True (contains "bcd" "abcd"))
conTestFalse1 = TestCase (assertEqual "for (contains \"a\" \"b\")," False (contains "a" "b"))
conTestFalse2 = TestCase (assertEqual "for (contains \"abc\" \"acbc\")," False (contains "abc" "acbc"))
conTestFalse3 = TestCase (assertEqual "for (contains \"ab\" \"b\")," False (contains "ab" "b"))

pfTests = TestList [
    TestLabel "prefix Test 1" pfTestEmpty,
    TestLabel "prefix Test 2" pfTestGood,
    TestLabel "prefix Test 3" pfTestBad,
    TestLabel "prefix Test 4" pfTestTrue1,
    TestLabel "prefix Test 5" pfTestTrue2,
    TestLabel "prefix Test 6" pfTestFalse1,
    TestLabel "prefix Test 7" pfTestFalse2
    ]
pfTestEmpty  = TestCase (assertEqual "for (prefix [] [])," True (prefix [] []))
pfTestGood   = TestCase (assertEqual "for (prefix [] \"a\")," True (prefix [] "a"))
pfTestBad    = TestCase (assertEqual "for (prefix \"a\" [])," False (prefix "a" []))
pfTestTrue1  = TestCase (assertEqual "for (prefix \"a\" \"abc\")," True (prefix "a" "abc"))
pfTestTrue2  = TestCase (assertEqual "for (prefix \"abc\" \"abc\")," True (prefix "abc" "abc"))
pfTestFalse1 = TestCase (assertEqual "for (prefix \"abcd\" \"abc\")," False (prefix "abcd" "abc"))
pfTestFalse2 = TestCase (assertEqual "for (prefix \"abc\" \"aabc\")," False (prefix "abc" "aabc"))

rsTests = TestList [
    TestLabel "removeStr Test 1" rsTest0Empty,
    TestLabel "removeStr Test 2" rsTest0,
    TestLabel "removeStr Test 3" rsTest1Empty,
    TestLabel "removeStr Test 4" rsTest1,
    TestLabel "removeStr Test 5" rsTest4,
    TestLabel "removeStr Test 6" rsTest4Short
    ]
rsTest0Empty = TestCase (assertEqual "for (removeStr 0 [])," "" (removeStr 0 []))
rsTest0      = TestCase (assertEqual "for (removeStr 0 \"abc\")," "abc" (removeStr 0 "abc"))
rsTest1Empty = TestCase (assertEqual "for (removeStr 1 [])," "" (removeStr 1 []))
rsTest1      = TestCase (assertEqual "for (removeStr 1 \"abc\")," "bc" (removeStr 1 "abc"))
rsTest4      = TestCase (assertEqual "for (removeStr 4 \"abcde\")," "e" (removeStr 4 "abcde"))
rsTest4Short = TestCase (assertEqual "for (removeStr 4 \"a\")," "" (removeStr 4 "a"))

ntcTests = TestList [
    TestLabel "numToCoords Test 1" ntcTestBad1,
    TestLabel "numToCoords Test 2" ntcTestBad2,
    TestLabel "numToCoords Test 3" ntcTestGood
    ]
ntcTestBad1 = TestCase (assertEqual "for (numToCoords 0 [])," (-1, -1) (numToCoords 0 []))
ntcTestBad2 = TestCase (assertEqual "for (numToCoords 3 [(0, 0)])," (-1, -1) (numToCoords 3 [(0, 0)]))
ntcTestGood = TestCase (assertEqual "for (numToCoords 2 [(0, 0), (1, 1), (2, 2), (3, 3)])," (2, 2) (numToCoords 3 [(0, 0), (1, 1), (2, 2), (3, 3)]))

gcTests = TestList [
    TestLabel "getCoords Test 1" gcTestEmpty,
    TestLabel "getCoords Test 2" gcTestOne,
    TestLabel "getCoords Test 3" gcTestSome,
    TestLabel "getCoords Test 4" gcTestMany
    ]
gcTestEmpty = TestCase (assertEqual "for (getCoords [] W (1, 1))," [] (getCoords [] W (1, 1)))
gcTestOne   = TestCase (assertEqual "for (getCoords [[W]] W (0, 0))," [(0, 0)] (getCoords [[W]] W (0, 0)))
gcTestSome  = TestCase (assertEqual "for (getCoords [[X, X], [B, W]] W (0, 0)),"
    [(1, 1)] (getCoords [[X, X], [B, W]] W (0, 0)))
gcTestMany  = TestCase (assertEqual "for (getCoords [[W, W], [W, X, W], [B, W]] W (0, 0)),"
    [(0, 0),(0, 1),(1, 0),(1, 2),(2, 1)] (getCoords [[W, W], [W, X, W], [B, W]] W (0, 0)))

gchTests = TestList [
    TestLabel "getCoordsHelper test 1" gchTestEmpty,
    TestLabel "getCoordsHelper test 2" gchTestOne,
    TestLabel "getCoordsHelper test 3" gchTestSome,
    TestLabel "getCoordsHelper test 4" gchTestMany
    ]
gchTestEmpty = TestCase (assertEqual "for (getCoordsHelper [] W (1, 1))," [] (getCoordsHelper [] W (1, 1)))
gchTestOne   = TestCase (assertEqual "for (getCoordsHelper [W] W (0, 0))," [(0, 0)] (getCoordsHelper [W] W (0, 0)))
gchTestSome  = TestCase (assertEqual "for (getCoordsHelper [X, X, B, W] W (1, 0)),"
    [(1, 3)] (getCoordsHelper [X, X, B, W] W (1, 0)))
gchTestMany  = TestCase (assertEqual "for (getCoordsHelper [W, W, W, X, W, B, W] W (2, 3)),"
    [(2, 3),(2, 4),(2, 5),(2, 7),(2, 9)]  (getCoordsHelper [W, W, W, X, W, B, W] W (2, 3)))

gpTests = TestList [
    TestLabel "getPos test 1" gpTestEmpty,
    TestLabel "getPos test 2" gpTest1Row,
    TestLabel "getPos test 3" gpTest2Row
    ]
gpTestEmpty = TestCase (assertEqual "for (getPos [] (1, 1))," P (getPos [] (1, 1)))
gpTest1Row  = TestCase (assertEqual "for (getPos [[W, W]] (0, 1))," W (getPos [[W, W]] (0, 1)))
gpTest2Row  = TestCase (assertEqual "for (getPos [[W, W], [X, X, X], [B, B]] (1, 1))," X (getPos [[W, W], [X, X, X], [B, B]] (1, 1)))

gphTests = TestList [
    TestLabel "getPosHelper test 1" gphTestEmpty,
    TestLabel "getPosHelper test 2" gphTestSmall,
    TestLabel "getPosHelper test 2" gphTestBig
    ]
gphTestEmpty = TestCase (assertEqual "for (getPosHelper [] 1)," P (getPosHelper [] 1))
gphTestSmall = TestCase (assertEqual "for (getPosHelper [W] 1)," P (getPosHelper [W] 1))
gphTestBig   = TestCase (assertEqual "for (getPosHelper [W, W] 1)," W (getPosHelper [W, W] 1))

spTests = TestList [
    TestLabel "setPos test 1" spTestEmpty,
    TestLabel "setPos test 2" spTest1Row,
    TestLabel "setPos test 3" spTest2Row
    ]
spTestEmpty = TestCase (assertEqual "for (setPos [] (1, 1) W)," [] (setPos [] (1, 1) W))
spTest1Row  = TestCase (assertEqual "for (setPos [[W, W]] (0, 1) B)," [[W, B]] (setPos [[W, W]] (0, 1) B))
spTest2Row  = TestCase (assertEqual "for (setPos [[W, W], [X, X, X], [B, B]] (1, 1) W),"
    [[W, W], [X, W, X], [B, B]] (setPos [[W, W], [X, X, X], [B, B]] (1, 1) W))

sphTests = TestList [
    TestLabel "setPosHelper test 1" sphTestEmpty,
    TestLabel "setPosHelper test 2" sphTestSmall,
    TestLabel "setPosHelper test 2" sphTestBig
    ]
sphTestEmpty = TestCase (assertEqual "for (setPosHelper [] 1 X)," [] (setPosHelper [] 1 X))
sphTestSmall = TestCase (assertEqual "for (setPosHelper [W] 1 X)," [W] (setPosHelper [W] 1 X))
sphTestBig   = TestCase (assertEqual "for (setPosHelper [W, W] 1 X)," [W, X] (setPosHelper [W, W] 1 X))
