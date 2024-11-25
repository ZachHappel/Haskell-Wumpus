module Main (main) where

import Lib
import Types

import Control.Monad.State


main :: IO ()
main = do
    -- Define the initial game state
  let initialState = GameState
        { playerState = Player 1 2 3, -- at pos 1, prev is 2, 3 arrows
          wumpusState = WumpusState 5, -- set wumpus pos @ 5 for now
          environment = EnvironmentState [], -- currently no hazards
          layout = decahedron -- cave layout
        }

  gameLoop initialState
    -- passes initialState to gameLoop and starts the loop

gameLoop :: GameState -> IO ()
gameLoop game = do
    -- game is passed into the function as an argument
  putStrLn caveArt
  print $ "Current Player State: " ++ show (playerState game)
  putStrLn "Enter your move (Left, Right, Back):"
  move <- getLine
  let parsedMove = case move of
        "Left"  -> Types.Left
        "Right" -> Types.Right
        "Back"  -> Types.Back
        _       -> error "Invalid move"
      updatedGame = execState (movePlayer parsedMove) game
  gameLoop updatedGame
    -- call gameLoop again, but now with the updated state

-- takes a Move, which it gets from gameLoops I/O and translates that into meaningful input
movePlayer :: Move -> State GameState ()
movePlayer direction = do
  -- access current game state
  game <- get
  let player@(Player current prev arrows) = playerState game
      gameLayout = layout game -- resolve issue with reuse of `layout`
      neighbors = getNeighbors current gameLayout
      adjustedNeighbors = getOrientationAdjustedNeighbors current prev gameLayout
      newPosition = case direction of
        Types.Back  -> adjustedNeighbors !! 0
        Types.Left  -> adjustedNeighbors !! 2
        Types.Right -> adjustedNeighbors !! 1
      updatedPlayer = Player newPosition current arrows
  -- update the game state with the new player state
  put $ game { playerState = updatedPlayer }
    -- returns the orginal game state with the updated values? 
