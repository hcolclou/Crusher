module Tests where

import Test.HUnit
import Piece
import CrusherBoard
import Util
import AI

allTests = TestList [pieceTests, cbTests, aiTests, utilTests]
