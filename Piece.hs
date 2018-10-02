module Piece where

-- DATA {

data Piece = W | B | X | P | Marker Int
    deriving (Eq)

-- } /DATA

-- TODO: write purposes for functions

-- PIECE FUNCTIONS {

getotherplayer :: Piece -> Piece
getotherplayer W = B
getotherplayer B = W

countall :: [[Piece]] -> Piece -> Int
countall [] _ = 0
countall (h:t) p = (count h p) + (countall t p)

count :: [Piece] -> Piece -> Int
count [] _ = 0
count (h:t) p
    | h == p    = 1 + count t p
    | otherwise = count t p

-- } /PIECE FUNCTIONS
