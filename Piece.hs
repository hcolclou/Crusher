module Piece (Board, Row, Point, Piece(W, B, X, P, Marker), getOtherPlayer, countAll,
    count, getDisplayChar, pieceTests) where

import Test.HUnit

data Piece = W | B | X | P | Marker Int
    deriving (Eq, Show)

type Board = [Row]
type Row = [Piece]
type Point = (Int, Int)


{-
Returns the opponent to the player given.
@param p The player to find the opponent for.
-}
getOtherPlayer :: Piece -> Piece
getOtherPlayer W = B
getOtherPlayer B = W
getOtherPlayer p = X

{-
Counts the number of Piece p in a list of a list of pieces.
@param (h:t) The board to count pieces on.
@param p     The piece type to count.
-}
countAll :: Board -> Piece -> Int
countAll [] _ = 0
countAll (h:t) p = (count h p) + (countAll t p)

{-
Counts the number of Piece p in a list of pieces.
@param (h:t) The row to count pieces on.
@param p     The piece type to count.
-}
count :: Row -> Piece -> Int
count [] _ = 0
count (h:t) p
    | h == p    = 1 + count t p
    | otherwise = count t p

{-
Describes how to display each type of Piece as a string.
@param p The piece type to get the display string for.
-}
getDisplayChar :: Piece -> String
getDisplayChar X = "-"
getDisplayChar W = "W"
getDisplayChar B = "B"
getDisplayChar P = "P"
getDisplayChar (Marker n) = show n




-- Piece Tests
pieceTests = TestList [opTests, caTests, cTests]

opTests = TestList [
    TestLabel "getOtherPlayer Test W" opTestW,
    TestLabel "getOtherPlayer Test B" opTestB,
    TestLabel "getOtherPlayer Test P" opTestP,
    TestLabel "getOtherPlayer Test X" opTestX,
    TestLabel "getOtherPlayer Test Marker" opTestM
    ]
opTestW = TestCase (assertEqual "for (getOtherPlayer W)," B (getOtherPlayer W))
opTestB = TestCase (assertEqual "for (getOtherPlayer B)," W (getOtherPlayer B))
opTestP = TestCase (assertEqual "for (getOtherPlayer P)," X (getOtherPlayer P))
opTestX = TestCase (assertEqual "for (getOtherPlayer X)," X (getOtherPlayer X))
opTestM = TestCase (assertEqual "for (getOtherPlayer (Marker 2))," X (getOtherPlayer (Marker 2)))

caTests = TestList [
    TestLabel "countAll Test 1" caTestEmpty,
    TestLabel "countAll Test 2" caTest2Empty,
    TestLabel "countAll Test 3" caTestSingle,
    TestLabel "countAll Test 4" caTestAllOther,
    TestLabel "countAll Test 5" caTestSomeGood
    ]
caTestEmpty    = TestCase (assertEqual "for (countAll [] W)," 0 (countAll [] W))
caTest2Empty   = TestCase (assertEqual "for (countAll [[], []] W)," 0 (countAll [[], []] W))
caTestSingle   = TestCase (assertEqual "for (countAll [[W]] W)," 1 (countAll [[W]] W))
caTestAllOther = TestCase (assertEqual "for (countAll [[B, X, P, (Marker 2)]] W)," 0 (countAll [[B, X, P, (Marker 2)]] W))
caTestSomeGood = TestCase (assertEqual "for (countAll [[W], [W], [B], [B]] W)," 2 (countAll [[W], [W], [B], [B]] W))

cTests = TestList [
    TestLabel "count Test 1" cTestEmpty,
    TestLabel "count Test 2" cTest1Good,
    TestLabel "count Test 3" cTest1Bad,
    TestLabel "count Test 4" cTestSomeGood
    ]
cTestEmpty    = TestCase (assertEqual "for (count [] W)," 0 (count [] W))
cTest1Good    = TestCase (assertEqual "for (count [W] W)," 1 (count [W] W))
cTest1Bad     = TestCase (assertEqual "for (count [B] W)," 0 (count [B] W))
cTestSomeGood = TestCase (assertEqual "for (count [W, B, W, B] W)," 2 (count [W, B, W, B] W))
