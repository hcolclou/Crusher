module CrusherBoard (baseState, generate, placeUserMarker, placePieceMarkers,
    getDisplayString, doMove, getMoves, getStomps, getSize, cbTests) where

import Test.HUnit
import Piece
import Util

baseState = [[X]]
{-
generate is a high level function that generates a new board with hexagonal side
length equal to the Int that it is given.
@param n The side length of the hexagonal board.
-}
generate :: Int -> Board
generate n
    | n > 1     = expandBoard baseState n 2
    | otherwise = baseState

{-
expandBoard takes a Board and two Ints and grows the given Board until the two
Ints are equal (we have reached the desired size).
@param board   The current board.
@param newSize The desired size of the board.
@param size    The current size of the board.
-}
expandBoard :: Board -> Int -> Int -> Board
expandBoard board newSize size
    | size == newSize = expandBoardHelper board newSize
    | otherwise       = expandBoard (expandBoardHelper board size) newSize (size + 1)

{-
expandBoardHelper is a helper function for expandBoard that takes a Board and an
Int and grows the Board a single time to make it have the size of the given Int.
@param board   The current board.
@param newSize The desired new size of the board.
-}
expandBoardHelper :: Board -> Int -> Board
expandBoardHelper board newSize
    = [(replicate newSize W)] ++ (extendLines board) ++ [(replicate newSize B)]

{-
extendLines takes a Board and adds one X to either side of each row in the
Board.
@param (h:t) The current board to be extended.
-}
extendLines :: Board -> Board
extendLines [] = []
extendLines (h:t) = ([X] ++ h ++ [X]):(extendLines t)

{-
placeUserMarker places a Piece of type P at the given Point on the given Board.
@param board The board to place the user marker on.
@param pos   The position to place the user marker.
-}
placeUserMarker :: Board -> Point -> Board
placeUserMarker board pos = setPos board pos P

{-
placePieceMarkers takes a list of Boards, a Board, and a Piece and returns a new
Board with all the Pieces of the given type changed to numbered Markers and a
list of Points that correspond to the Markers.
@param visited The list of boards that have already existed.
@param board   The board to place user markers on.
@param p       The piece type to switch for user markers.
-}
placePieceMarkers :: [Board] -> Board -> Piece -> (Board, [Point])
placePieceMarkers visited board p =
    (newboard, (getCoords board p (0, 0)))
    where newboard = placePieceMarkersHelper visited board board p 1 (0, 0)

{-
placePieceMarkersHelper is a helper function for placePieceMarkers that takes a
list of Boards, two Boards, a Piece, an Int, and a Point and returns a new Board
with all the Pieces of the given type changed to numbered Markers.
@param visited The list of boards that have already existed.
@param board   The board to place the user markers on.
@param (h:t)   The board-so-far after replacing pieces.
@param p       The piece type to replace.
@param n       The number of the current piece.
@param (y, x)  The current position on the board.
-}
placePieceMarkersHelper :: [Board] -> Board -> Board -> Piece -> Int -> Point -> Board
placePieceMarkersHelper _ _ [] _ _ _ = []
placePieceMarkersHelper visited board (h:t) p n (y, 0) =
    (placePieceMarkersLine visited board h p n (y, 0)):(placePieceMarkersHelper visited board t p (n + (count h p)) (y + 1, 0))

{-
placePieceMarkersLine is a helper function for placePieceMarkersHelper that
takes a list of Boards, a Board, a Row, a Piece, an Int, and  Point and returns
a new Row with all the Pieces of the given type replaced with numbered Markers.
@param visited The list of boards that have already existed.
@param board   The board to place user markers on.
@param (h:t)   The row-so-far after replacing some pieces.
@param p       The piece type to replace.
@param n       The number of the current piece.
@param (y, x)  The current position on the board.
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
@param (h:t) Board to get display string for.
@param n     The amount of padding to use.
-}
getDisplayString :: Board -> Int -> String
getDisplayString [] _ = ""
getDisplayString (h:t) n =
    (replicate (abs n) ' ') ++ (getDisplayStringLine h) ++ "\n" ++ (getDisplayString t (n - 1))

{-
getDisplayStringLine is a helper function for getDisplayString that gets the
String that represents a single Row of the overall Board.
@param (h:t) Row to get display string for.
-}
getDisplayStringLine :: Row -> String
getDisplayStringLine []    = ""
getDisplayStringLine (h:t) = (getDisplayChar h) ++ " " ++ (getDisplayStringLine t)

{-
doMove takes a Board and two Points and moves copies the Piece at the first
Point to the second Point, leaving an X where it was previously.
@param board Board to do the move on.
@param old   Old position of piece.
@param new   New position of piece.
-}
doMove :: Board -> Point -> Point -> Board
doMove board old new = setPos (setPos board new (getPos board old)) old X

{-
getMoves takes a list of Boards, a Board, and a Point and returns a new Board
with numbered Markers on the Points where a Piece at the given Point can move
as well as a list of the Points it can move to.
@param visited The list of boards that have already existed.
@param board   Board to get moves from.
@param pos     Position of piece to get moves for.
-}
getMoves :: [Board] -> Board -> Point -> (Board, [Point])
getMoves visited board pos = (getMovesLoop board 1 moves, moves)
    where moves = getMoveSpaces visited board pos

{-
getMovesLoop takes a Board, and Int, and a list of Points and returns a Board
with each Point being set to a Marker with a unique number.
@param board Board to get moves from.
@param n     Number of the current move.
@param (h:t) Point that is currently being marked.
-}
getMovesLoop :: Board -> Int -> [Point] -> Board
getMovesLoop board _ [] = board
getMovesLoop board n (h:t)
    | (getPos board h) == X = getMovesLoop (setPos board h (Marker n)) (n + 1) t
    | otherwise             = getMovesLoop board n t
{-
getMoveSpaces is a function that takes a list of Boards, a Board, and a Point
and returns a list of Points to which the Piece at the given Point can move to.
@param visited The list of boards that have already existed.
@param board   Board to get moves from.
@param (y, x)  Point to check for moves from.
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
@param visited The list of boards that have already existed.
@param board   Board to get possible stomps from.
@param p       Piece type of current player.
@param pos     Point to check for stomps from.
-}
getStomps :: [Board] -> Board -> Piece -> Point -> (Board, [Point])
getStomps visited board p pos = (getStompsLoop board p 1 stomps, stomps)
    where stomps = getStompSpaces visited board p pos

{-
getStompsLoop takes a Board, a Piece, an Int, and a List of Points and returns
a new Board with all the possible stomp locations in the list of Points replaced
by numbered Markers.
@param board Board to get stomps from.
@param p     Piece type of current player.
@param n     Number of current stomp.
@param (h:t) List of points to number.
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
@param visited The list of boards that have already existed.
@param board   Board to get stomps from.
@param p       Piece type of current player.
@param (y, x)  Point to get stomps from.
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
@param visited The list of boards that have already existed.
@param board   Board to validate spaces for.
@param p       List of piece types that it cannot move/stomp to.
@param old     Old point of the moving/stomping piece.
@param (h:t)   List of possible points to move/stomp to.
-}
validateSpaces :: [Board] -> Board -> [Piece] -> Point -> [Point] -> [Point]
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
@param visited The list of boards that have already existed.
@param board   Board to validate spaces for.
@param p       List of piece types that it cannot move/stomp to.
@param (y, x)  Point to validate.
@param sizes   Lengths of the rows.
@param old     Old point of moving/stomping piece.
-}
validSpace :: [Board] -> Board -> [Piece] -> Point -> [Int] -> Point -> Bool
validSpace visited board p (y, x) sizes old =
    y >= 0                                          -- y is non-negative
    && y < (length sizes)                           -- y is smaller than the max
    && x >= 0                                       -- x is non-negative
    && x < (sizes!!y)                               -- x is smaller than the max
    && not (elem (board!!y!!x) p)                   -- new position is not help by piece
    && not (elem (doMove board old (y, x)) visited) -- board has not existed before

{-
getSize takes a Board and returns the size that was used to generate it.
@param b Board to get the size of. Must have odd number of rows.
-}
getSize :: Board -> Int
getSize b = div (length b + 1) 2

-- CrusherBoard Tests
cbTests = TestList [gTests, ebTests, ebhTests, ebhTests, elTests, pumTests,
    ppmTests, ppmhTests, ppmlTests, gdsTests, gdslTests, dmTests, gmTests,
    gmlTests, gmsTests, gsTests, gslTests, gssTests, vtsTests, vsTests, gszTests]

gTests = TestList [
    TestLabel "generate test 1" gTestBase,
    TestLabel "generate test 2" gTestMore
    ]

gTestBase = TestCase (assertEqual "for (generate 1)," baseState (generate 1))
gTestMore = TestCase (assertEqual "for (generate 3),"
    [[W, W, W], [X, W, W, X], [X, X, X, X, X], [X, B, B, X], [B, B, B]] (generate 3))

ebTests = TestList [
    TestLabel "expandBoard test 1" ebTestEmpty
    ]

ebTestEmpty = TestCase (assertEqual "for (expandBoard [[X]] 1 1)," [[W],[X,X,X],[B]] (expandBoard [[X]] 1 1))

ebhTests = TestList [
    TestLabel "expandBoardHelper test 1" ebhTestEmpty
    ]

ebhTestEmpty = TestCase (assertEqual "for (expandBoardHelper [[X]] 1)," [[W,W],[X,X,X],[B,B]] (expandBoardHelper [[X]] 2))

elTests = TestList [
    TestLabel "extendLines test 1" ebhTestBase,
    TestLabel "extendLines test 2" ebhTestHigher
    ]

ebhTestBase = TestCase (assertEqual "for (extendLines [[X]])," [[X, X, X]] (extendLines [[X]]))
ebhTestHigher = TestCase (assertEqual "for (extendLines [[X], [X, X, X], [X]]),"
    [[X, X, X], [X, X, X, X, X], [X, X, X]] (extendLines [[X], [X, X, X], [X]]))

pumTests = TestList [
    TestLabel "placeUserMarker test 1" pumTestBase
    ]

pumTestBase = TestCase (assertEqual "for (placeUserMarker [[W]] (0,0))," [[P]] (placeUserMarker [[W]] (0,0)))

ppmTests = TestList [
    TestLabel "placePieceMarkers test 1" ppmTestBase,
    TestLabel "placePieceMarkers test 2" ppmTestWhite,
    TestLabel "placePieceMarkers test 3" ppmTestBlack
    ]

ppmTestBase  = TestCase (assertEqual "for (placePieceMarkers [] [] W),"
    ([],[]) (placePieceMarkers [] [] W))
ppmTestWhite = TestCase (assertEqual "for (placePieceMarkers [] [[W,W],[X,X,X],[B,B]] W),"
    ([[(Marker 1),(Marker 2)],[X,X,X],[B,B]],[(0,0),(0,1)]) (placePieceMarkers [] [[W,W],[X,X,X],[B,B]] W))
ppmTestBlack = TestCase (assertEqual "for (placePieceMarkers [] [[W,X,B]] B),"
    ([[W,X,(Marker 1)]],[(0,2)]) (placePieceMarkers [] [[W,X,B]] B))

ppmhTests = TestList [
    TestLabel "placePieceMarkersHelper test 1" ppmhTestBase,
    TestLabel "placePieceMarkersHelper test 2" ppmhTestMore
    ]

ppmhTestBase = TestCase (assertEqual "for (placePieceMarkersHelper [] [[W,X,B]] [[W,X,B]] B 2 (0,0)),"
    [[W,X,(Marker 2)]] (placePieceMarkersHelper [] [[W,X,B]] [[W,X,B]] B 2 (0,0)))
ppmhTestMore = TestCase (assertEqual "for (placePieceMarkersHelper [] [[W,X,B]] [[W,X,B]] B 2 (0,0)),"
    [[X,W],[W,X,(Marker 2)],[X,(Marker 3)]] (placePieceMarkersHelper [] [[X,W],[W,X,B],[X,B]] [[X,W],[W,X,B],[X,B]] B 2 (0,0)))

ppmlTests = TestList [
    TestLabel "placePieceMarkersLine test 1" ppmlTestCase
    ]

ppmlTestCase = TestCase (assertEqual "for (placePieceMarkersLine [] [W,X,X,B,X,X,B] [W,X,X,B,X,X,B] B 4 (0,0)),"
    [X,(Marker 4),X] (placePieceMarkersLine [] [[W,X],[X,B,X],[X,B]] [X,B,X] B 4 (1,0)))

gdsTests = TestList [
    TestLabel "getDisplayString test 1" gdsTestCase
    ]

gdsTestCase = TestCase (assertEqual "for (getDisplayString [[W,W],[X,X,X],[B,B]] 1)," " W W \n- - - \n B B \n" (getDisplayString [[W,W],[X,X,X],[B,B]] 1))

gdslTests = TestList [
    TestLabel "getDisplayStringLine test 1" gdslTestCase
    ]

gdslTestCase = TestCase (assertEqual "for (getDisplayStringLine [W,B,X,P,(Marker 8)])," "W B - P 8 " (getDisplayStringLine [W,B,X,P,(Marker 8)]))

dmTests = TestList [
    TestLabel "doMove test 1" dmTestW1,
    TestLabel "doMove test 2" dmTestB2
    ]

dmTestW1 = TestCase (assertEqual "for (doMove [[X, W]] (1, 2) (1, 1))," [[W, X]] (doMove [[X, W]] (0, 1) (0, 0)))
dmTestB2 = TestCase (assertEqual "for (doMove [[B, X]] (1, 1) (1, 2))," [[X, B]] (doMove [[B, X]] (0, 0) (0, 1)))

gmTests = TestList [
    TestLabel "getMoves test 1" gmTestSingle,
    TestLabel "getMoves test 2" gmTestMulti
    ]

gmTestSingle = TestCase (assertEqual "for (gmTestSingle [] [[W, X]] (0, 0)),"
    ([[W,W],[(Marker 1),(Marker 2),X],[B,B]], [(1,0),(1,1)])
    (getMoves [] [[W,W],[X,X,X],[B,B]] (0, 0)))
gmTestMulti = TestCase (assertEqual "for (gmTestMulti [] [[X, X], [X, W, X], [X, X]] (2, 2)),"
    ([[(Marker 1), (Marker 2)], [(Marker 3), W, (Marker 4)], [(Marker 5), (Marker 6)]],
        [(0, 0), (0, 1), (1, 0), (1, 2), (2, 0), (2, 1)])
    (getMoves [] [[X, X], [X, W, X], [X, X]] (1, 1)))

gmlTests = TestList [
    TestLabel "getMovesLoop test 1" gmlTestCase
    ]

gmlTestCase = TestCase (assertEqual "for (getMovesLoop [[X,X],[W,W,B],[X,X]] 1 [(0,0),(0,1),(2,0),(2,1)]),"
    [[(Marker 1),(Marker 2)],[W,W,B],[(Marker 3),(Marker 4)]](getMovesLoop [[X,X],[W,W,B],[X,X]] 1 [(0,0),(0,1),(2,0),(2,1)]))

gmsTests = TestList [
    TestLabel "getMovesLoop test 1" gmsTestCase
    ]

gmsTestCase = TestCase (assertEqual "for (getMoveSpaces [] [[X,X],[W,W,B],[X,B]] (1,1))," [(0,0),(0,1),(2,0)] (getMoveSpaces [] [[X,X],[W,W,B],[X,B]] (1,1)))

gsTests = TestList [
    TestLabel "getMovesLoop test 1" gsTestCase
    ]

gsTestCase = TestCase (assertEqual "for (getStomps [] [[W,X],[X,X,X],[X,B]] W (0,0)),"
    ([[W,X],[X,X,X],[X,(Marker 1)]], [(2,1)]) (getStomps [] [[W,X],[X,X,X],[X,B]] W (0,0)))

gslTests = TestList [
    TestLabel "getMovesLoop test 1" gslTestCase
    ]

gslTestCase = TestCase (assertEqual "for (getStompsLoop [[W,X],[X,X,X],[X,B]] W 1 [(2,1)]),"
    [[W,X],[X,X,X],[X,(Marker 1)]] (getStompsLoop [[W,X],[X,X,X],[X,B]] W 1 [(2,1)]))

gssTests = TestList [
    TestLabel "getMovesLoop test 1" gssTestCase
    ]

gssTestCase = TestCase (assertEqual "for (getStompSpaces [] [[W,W],[X,X,X],[X,B]] W (0,0)),"
    [(2,1)] (getStompSpaces [] [[W,W],[X,X,X],[X,B]] W (0,0)))

vtsTests = TestList [
    TestLabel "validateSpaces test 1" vtsTestEmpty,
    TestLabel "validateSpaces test 2" vtsTestSingle,
    TestLabel "validateSpaces test 3" vtsTestFilter
    ]
vtsTestEmpty  = TestCase (assertEqual "for (validateSpaces [] [] [] (1, 1) []),"
    [] (validateSpaces [] [] [] (1, 1) []))
vtsTestSingle = TestCase (assertEqual "for (validateSpaces [] [] [] (1, 1) []),"
    [(1, 0), (1, 2)] (validateSpaces [] [[X, X], [X, X, X], [X, X]] [] (1, 1) [(1, 0), (1, 2)]))
vtsTestFilter = TestCase (assertEqual "for (validateSpaces [] [] [] (1, 1) []),"
    [(1, 0)] (validateSpaces [] [[X, X], [X, X, W], [X, X]] [W] (1, 1) [(1, 0), (1, 2)]))

vsTests = TestList [
    TestLabel "validSpace test 1" vsTestEmpty,
    TestLabel "validSpace test 2" vsTestVisited,
    TestLabel "validSpace test 3" vsTestTooBig,
    TestLabel "validSpace test 4" vsTestOtherP,
    TestLabel "validSpace test 5" vsTestValid
    ]
vsTestEmpty   = TestCase (assertEqual "for (validSpace [] [] [] (1, 1) [] (2, 2)),"
    False (validSpace [] [] [] (1, 1) [] (2, 2)))
vsTestVisited = TestCase (assertEqual "for (validSpace [[[X, X, X], [X, X, X], [X, X, X]]] [[X, X, X], [X, X, X], [X, X, X]] [X, X, X] (1, 1) [3, 3, 3] (2, 2)),"
    False (validSpace [[[X, X, X], [X, X, X], [X, X, X]]] [[X, X, X], [X, X, X], [X, X, X]] [X, X, X] (1, 1) [3, 3, 3] (2, 2)))
vsTestTooBig  = TestCase (assertEqual "for (validSpace [] [[X]] [X] (10, 10) [1] (20, 20)),"
    False (validSpace [] [[X]] [X] (10, 10) [1] (20, 20)))
vsTestOtherP  = TestCase (assertEqual "for (validSpace [] [[W, B]] [W, B] (0, 0) [2] (0, 1)),"
    False (validSpace [] [[W, B]] [W, B] (0, 0) [2] (0, 1)))
vsTestValid   = TestCase (assertEqual "for (validSpace [] [[W, X]] [W, B] (0, 1) [2] (0, 0)),"
    True (validSpace [] [[W, X]] [W, B] (0, 1) [2] (0, 0)))

gszTests = TestList [
    TestLabel "getSize test 1" gszTest1,
    TestLabel "getSize test 2" gszTest2,
    TestLabel "getSize test 3" gszTest3
    ]
gszTest1 = TestCase (assertEqual "for (getSize [[]])," 1 (getSize [[]]))
gszTest2 = TestCase (assertEqual "for (getSize [[], [], []]),"
    2 (getSize [[], [], []]))
gszTest3 = TestCase (assertEqual "for (getSize [[], [], [], [], []]),"
    3 (getSize [[], [], [], [], []]))
