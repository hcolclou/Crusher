module GUI where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk 
import Piece
import Crusher 


initializeWindow :: IO ()
initializeWindow = 
	do
	  initGUI               -- (1)
	  window <- windowNew   -- (2)
	  set window [ windowTitle         := "Crusher"
	             , windowResizeable    := True
	             , windowDefaultWidth  := 500
	             , windowDefaultHeight := 500 ]
	  display <- entryNew
	  if (createWelcomeMessage "Do you want to play?") 'elem' yes
	  	then 
	  		do 
	  			set display [ entryEditable := True                     -- TODO
	    	   		        , entryText     := Crusher ]
	  			parseBoard  
	    else 
	    	do 
	    		if (set display [ entryEditable := False
	    					, entryText     := dialogNew "Oh okay"]
	  widgetShowAll window
	  mainGUI               -- (5)


mkButton :: String -> (Maybe IO())
mkButton label = 
	do
	  btn <- buttonNew
	  set btn [ buttonLabel := label ]
	  return btn


createWelcomeMessage:: String -> IO Dialog
createWelcomeMessage msg = dialogNewWithButtons msg yesb nob
								where 
									yesb = dialogAddButton MessageInfo "Ok" ResponseYes 
									nob  = dialogAddButton MessageInfo "Cancel" ResponseNo 


-- parses the board
parseBoard :: Board -> IO
parseBoard []  = []
parseBoard (h:t) = parsePiece h : parseBoard t 


-- Helper function which creates the buttons 
parsePiece :: [Piece] -> [IO]  
parsePiece [] = []
parsePiece (h:t) 
	|h == W      = mkButton "W" : parsePiece t
	|h == B      = mkButton "B" : parsePiece t
	|otherwise   = mkButton "-" : parsePiece t
 