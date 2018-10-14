module CrusherBoard (baseState, generate, placeUserMarker, placePieceMarkers,
    getDisplayString, doMove, getMoves, getStomps, getSize, cbTests) where

import Test.HUnit
import Piece
import Util

baseState = [[X]]
{-
generate is a high level function that generates a new board with hexagonal side
length equal to the Int that it is given.
-}
generate :: Int -> Board
generate n
    | n > 1     = expandBoard baseState n 2
    | otherwise = baseState

{-
expandBoard takes a Board and two Ints and grows the given Board until the two
Ints are equal (we have reached the desired size).
-}
expandBoard :: Board -> Int -> Int -> Board
expandBoard board newSize size
    | size == newSize = expandBoardHelper board newSize
    | otherwise       = expandBoard (expandBoardHelper board size) newSize (size + 1)

{-
expandBoardHelper is a helper function for expandBoard that takes a Board and an
Int and grows the Board a single time to make it have the size of the given Int.
-}
expandBoardHelper :: Board -> Int -> Board
expandBoardHelper board newSize
    = [(replicate newSize W)] ++ (extendLines board) ++ [(replicate newSize B)]

{-
extendLines takes a Board and adds one X to either side of each row in the
Board.
-}
extendLines :: Board -> Board
extendLines [] = []
extendLines (h:t) = ([X] ++ h ++ [X]):(extendLines t)

{-
placeUserMarker places a Piece of type P at the given Point on the given Board.
-}
placeUserMarker :: Board -> Point -> Board
placeUserMarker board pos = setPos board pos P

{-
placePieceMarkers takes a list of Boards, a Board, and a Piece and returns a new
Board with all the Pieces of the given type changed to numbered Markers and a
list of Points that correspond to the Markers.
-}
placePieceMarkers :: [Board] -> Board -> Piece -> (Board, [Point])
placePieceMarkers visited board p =
    (newboard, (getCoords board p (0, 0)))
    where newboard = placePieceMarkersHelper visited board board p 1 (0, 0)

{-
placePieceMarkersHelper is a helper function for placePieceMarkers that takes a
list of Boards, two Boards, a Piece, an Int, and a Point and returns a new Board
with all the Pieces of the given type changed to numbered Markers.
-}
placePieceMarkersHelper :: [Board] -> Board -> Board -> Piece -> Int -> Point -> Board
placePieceMarkersHelper _ _ [] _ _ _ = []
placePieceMarkersHelper visited board (h:t) p n (y, 0) =
    (placePieceMarkersLine visited board h p n (y, 0)):(placePieceMarkersHelper visited board t p (n + (count h p)) (y + 1, 0))

{-
placePieceMarkersLine is a helper function for placePieceMarkersHelper that
takes a list of Boards, a Board, a Row, a Piece, an Int, and  Point and returns
a new Row with all the Pieces of the given type replaced with numbered Markers.
-}
placePieceMarkersLine :: [Board] -> Board -> Row -> Piece -> Int -> Point -> Row
placePieceMarkersLine _ _ [] _ _ _ = []
placePieceMarkersLine visited board (h:t) p n (y, x)
    | h == p && ((snd (getMoves visited board (y, x))) ++
                (snd (getStomps visited board p (y, x)))) /= [] =
        (Marker n):(placePieceMarkersLine visited board t p (n + 1) (y, x + 1))
    | otherwise                                    = h:(placePieceMarkersLine visited board t p n (y, x + 1))

{-
Given a Board and an Int (the size minus 1), getDisplayString will return a
String that represents the Board and can be displayed on the command line.
-}
getDisplayString :: Board -> Int -> String
getDisplayString [] _ = ""
getDisplayString (h:t) n =
    (replicate (abs n) ' ') ++ (getDisplayStringLine h) ++ "\n" ++ (getDisplayString t (n - 1))

{-
getDisplayStringLine is a helper function for getDisplayString that gets the
String that represents a single Row of the overall Board.
-}
getDisplayStringLine :: Row -> String
getDisplayStringLine []    = ""
getDisplayStringLine (h:t) = (getDisplayChar h) ++ " " ++ (getDisplayStringLine t)

{-
doMove takes a Board and two Points and moves copies the Piece at the first
Point to the second Point, leaving an X where it was previously.
-}
doMove :: Board -> Point -> Point -> Board
doMove board old new = setPos (setPos board new (getPos board old)) old X

{-
getMoves takes a list of Boards, a Board, and a Point and returns a new Board
with numbered Markers on the Points where a Piece at the given Point can move
as well as a list of the Points it can move to.
-}
getMoves :: [Board] -> Board -> Point -> (Board, [Point])
getMoves visited board pos = (getMovesLoop board 1 moves, moves)
    where moves = getMoveSpaces visited board pos

{-
getMovesLoop takes a Board, and Int, and a list of Points and returns a Board
with each Point being set to a Marker with a unique number.
-}
getMovesLoop :: Board -> Int -> [Point] -> Board
getMovesLoop board _ [] = board
getMovesLoop board n (h:t)
    | (getPos board h) == X = getMovesLoop (setPos board h (Marker n)) (n + 1) t
    | otherwise             = getMovesLoop board n t
{-
getMoveSpaces is a function that takes a list of Boards, a Board, and a Point
and returns a list of Points to which the Piece at the given Point can move to.
-}
getMoveSpaces :: [Board] -> Board -> Point -> [Point]
getMoveSpaces visited board (y, x)
    | y == getSize board - 1 =
        validateSpaces visited board ps (y, x)
            [(y - 1, x - 1), (y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x - 1), (y + 1, x)]
    | y < getSize board - 1 =
        validateSpaces visited board ps (y, x)
            [(y - 1, x - 1), (y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x), (y + 1, x + 1)]
    | otherwise =
        validateSpaces visited board ps (y, x)
            [(y - 1, x), (y - 1, x + 1), (y, x - 1), (y, x + 1), (y + 1, x - 1), (y + 1, x)]
    where ps = [W, B]

{-
getStomps takes a list of Boards, a Board, a Piece, and a Point and returns a
new Board with all the possible stomps from the given Point by the given Piece
as well as a list of Points corresponding to each stomp.
-}
getStomps :: [Board] -> Board -> Piece -> Point -> (Board, [Point])
getStomps visited board p pos = (getStompsLoop board p 1 stomps, stomps)
    where stomps = getStompSpaces visited board p pos

{-
getStompsLoop takes a Board, a Piece, an Int, and a List of Points and returns
a new Board with all the possible stomp locations in the list of Points replaced
by numbered Markers.
-}
getStompsLoop :: Board -> Piece -> Int -> [Point] -> Board
getStompsLoop board _ _ [] = board
getStompsLoop board p n (h:t)
    | elem (getPos board h) stompon = getStompsLoop (setPos board h (Marker n)) p (n + 1) t
    | otherwise                     = getStompsLoop board p n t
    where stompon = [(getOtherPlayer p), X]

{-
getStompSpaces takes a list of Boards, a Board, a Piece, and a Point and returns
a list of Points to which the Piece at the given Point can stomp on the given
Board.
-}
getStompSpaces :: [Board] -> Board -> Piece -> Point -> [Point]
getStompSpaces visited board p (y, x)
    | y < getSize board - 2 =
        validateSpaces visited board ps (y, x)
            [(y - 2, x - 2), (y - 2, x), (y, x - 2), (y, x + 2), (y + 2, x), (y + 2, x + 2)]
    | y == getSize board - 2 =
        validateSpaces visited board ps (y, x)
            [(y - 2, x - 2), (y - 2, x), (y, x - 2), (y, x + 2), (y + 2, x - 1), (y + 2, x + 1)]
    | y == getSize board - 1 =
        validateSpaces visited board ps (y, x)
            [(y - 2, x - 2), (y - 2, x), (y, x - 2), (y, x + 2), (y + 2, x - 2), (y + 2, x)]
    | y == getSize board =
        validateSpaces visited board ps (y, x)
            [(y - 2, x - 1), (y - 2, x + 1), (y, x - 2), (y, x + 2), (y + 2, x - 2), (y + 2, x)]
    | otherwise =
        validateSpaces visited board ps (y, x)
            [(y - 2, x), (y - 2, x + 2), (y, x - 2), (y, x + 2), (y + 2, x - 2), (y + 2, x)]
    where ps = [p]

{-
validateSpaces takes a list of Boards, a Board, a Row, a Point, and a list of
Points and returns a list of Points which is the same as the given list of
Points except that all invalid stomps (out of bounds, on to self, Board already
existed) have been removed.
-}
validateSpaces :: [Board] -> Board -> Row -> Point -> [Point] -> [Point]
validateSpaces _ _ _ _ [] = []
validateSpaces visited board p old (h:t)
    | validSpace visited board p h ([size..(2 * size - 2)] ++ [(2 * size - 1)] ++
          reverse [size..(2 * getSize board - 2)]) old
        = h:(validateSpaces visited board p old t)
    | otherwise
        = validateSpaces visited board p old t
    where size = getSize board

{-
validSpace takes a list of Boards, a Board, a Row, two Points, and a list of
Ints and returns whether it is valid for the first Point to move or stomp to
the second Point.
-}
validSpace :: [Board] -> Board -> Row -> Point -> [Int] -> Point -> Bool
validSpace visited board p (y, x) sizes old =
    y >= 0 && y < length sizes && x >= 0 && x < sizes!!y && not (elem (board!!y!!x) p)
    && not (elem (doMove board old (y, x)) visited)

{-
getSize takes a Board and returns the size that was used to generate it.
-}
getSize :: Board -> Int
getSize b = div (length b + 1) 2

-- CrusherBoard Tests
cbTests = TestList []
