module Main (main) where

import Lib
import Types
import Functions

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
  --putStrLn $ formatPlayerState (playerState game)
  putStrLn menuHeader
  putStrLn $ formatPlayerStateBetter (playerState game)
  putStrLn menuBody
  -- option to Shoot arrow? 



  input <- getLine
  let parsedInput = case input of
        "Left"  -> Just $ Movement Types.Left
        "Right" -> Just $ Movement Types.Right
        "Back"  -> Just $ Movement Types.Back
        "Smell" -> Just $ Action "Smell"
        "Listen" -> Just $ Action "Listen"
        "Shoot Left" -> Just $ Action "Shoot Left"
        "Shoot Right" -> Just $ Action "Shoot Right"
        _       -> Nothing-- error "Invalid move"
    -- call gameLoop again, but now with the updated state

  case parsedInput of
    Just (Movement move) -> do
      let updatedGame = execState (movePlayer move) game
      gameLoop updatedGame
    Just (Action "Smell") -> do
      putStrLn $ smell game
      gameLoop game
    Just (Action "Listen") -> do
      putStrLn $ listen game
      gameLoop game
    Just (Action action) -> do
      putStrLn $ "Unknown action: " ++ action
      gameLoop game
    Nothing -> do
      putStrLn "Invalid input. Try again."
      gameLoop game         
  --gameLoop updatedGame
      --updatedGame = execState (movePlayer parsedMove) game
-- takes a Move, which it gets from gameLoops I/O and translates that into meaningful input




movePlayer :: Move -> State GameState ()
movePlayer direction = do
  -- access current game state
  game <- get
  let player@(Player current prev arrows) = playerState game
      {-
        `playerState game` retrieves/extracts playerState from the GameState, `game`
        
        On the left side within the parenthesis, we have (Player current prev arrows)
          What this is doing is pattern-matching the structure of what is returned on the right side of the assignment
          `Player` deconstructs playerState into its individual components; 
              current (playerPosition), 
              prev (lastPosition),
              arrows (playerArrowCount)

        The `player@(...)` syntax binds the entire value contained within the parenthesis to the variable `player`
        This is not copying, or creating a new variable in the sense of duplicating memory,
          This is a `name binding` which refers to the entire value of `playerState game`
          `player` refers back directly to the original value
        

      -}
      gameLayout = layout game -- resolve issue with reuse of `layout`
      neighbors = getNeighbors current gameLayout
      adjustedNeighbors = getOrientationAdjustedNeighbors current prev gameLayout

      -- get Senses 
      

      newPosition = case direction of
        Types.Back  -> adjustedNeighbors !! 0
        Types.Left  -> adjustedNeighbors !! 2
        Types.Right -> adjustedNeighbors !! 1
      updatedPlayer = Player newPosition current arrows
  -- update the game state with the new player state
  put $ game { playerState = updatedPlayer }
    -- returns the orginal game state with the updated values? 



{-

  putStrLn "Choose your action (Move, Sense, Shoot):\n "
  action <- getLine
  let parsedAction = case action of
    -- shoot, smell, feel, listen. How do we get info about gameState. 
        "Move"  -> Types.Left
        "Sense" -> Types.Right
        "Shoot"  -> Types.Back
        _       -> error "Invalid action"
  



-}


{- Original move input parsing

let parsedMove = case move of
    -- shoot, smell, feel, listen. How do we get info about gameState. 
        "Left"  -> Types.Left
        "Right" -> Types.Right
        "Back"  -> Types.Back
        _       -> error "Invalid move"
      updatedGame = execState (movePlayer parsedMove) game
  gameLoop updatedGame
-}