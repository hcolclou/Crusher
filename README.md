# Crusher

A git repository for working on our CPSC 312 project, which is implementing a version of the game Crusher on the command
line.

Extra parts: 
    - The AI player will calculate the odds of winning for each move it can make, and makes the one with the
      highest probability. The AI will use heuristics to estimate the value of each possible move with a limited depth to
      gain it some speed. 
    - The AI will also use some parallel processing to speed itself up. A quick test showed the parallel processing to be 
      successful. When only changing one function (calculateOddsMult) to use it, the AI's first move on a size 3 board with 
      depth 3 went from taking around 8 seconds to around 2 seconds. Nearly all following moves are quicker as the number 
      of possible moves tends to decrease.
    - The AI keeps track of all the board states that have existed and associated them with numbers. Each board's number is 
      calculated by the number of times W has won where at some point during that mach this board existed minus the number
      of times B has won where at some point during that match this board existed. It then takes this into account during
      heuristics step to help determine whether a move is good or not. Manual testing has shown that pitting AI vs. AI with 
      this enabled leads to almost every game being different and the winner not always being the same.

How to use:
    - If you want to play the game, set 'ai' in Crusher.hs to be the Piece type that you would like the AI to play as,
      then compile the code using ':l Crusher' and run it using 'play <d> <n>' where n is the size of the board you would 
      like and d is the 'Brain' to start with. Note that larger boards will make the AI slow down in its decision making. 
      Ex:
        ':l Crusher'
        'play mempty 3'
    - If you want to run the tests, compile the code using ':l Tests' and run them all using 'runTestTT allTests' or run
      the tests for a single file using 'runTestTT <tests>' where tests is the name of the list of tests corresponding to
      the file you want to test. Here is the list of individual file test suites:
        - Piece.hs:        pieceTests
        - Util.hs:         utilTests
        - AI.hs:           aiTests
        - CrusherBoard.hs: cbTests
