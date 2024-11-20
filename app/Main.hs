module Main where

import Data.Char (toLower)
import Data.List (break)

-- import Lib
-- import GameLogic (Action, GameState, gameLoop, initialState, isGameOver) TODO: Example parameters for gameLogic function

main :: IO ()
main = do
  putStrLn "Welcome Hunt the Wumpus!"
  gameLoopIO initialState -- Begin game loop with init state

gameLoopIO :: GameState -> IO ()
gameLoopIO state = do
  putStrLn $ showState state -- Display the init state
  -- TODO: You're in cave #n
  putStrLn "Enter move"
  input <- getLine
  case parseInput input of -- cases for what to do when the input is valid
    Just action -> do
      -- in the case the action is valid
      let newState = applyAction state action -- create the new game state with the curr state and action
      if isGameOver newState -- if this resulting game state exists and results in a death
        then putStrLn "Game over"
        else gameLoopIO newState -- else, continue game loop
    Nothing -> do
      -- in the case the action is invalid
      putStrLn "Invalid action"
      gameLoopIO state -- continue the game loop re-asking "Enter move"

parseInput :: String -> Maybe Action
parseInput input =
  let (cmd, dir) = break (== ' ') input -- command, direction
    (command, direction) = $ map toLower cmd, dropWhile (== ' ') (map toLower rest)
   in case command of
        "move" -> case direction of
          "left" -> Just Move left -- TODO: move left
          "right" -> Just Move right -- TODO: move right
          "back" -> Just Move back -- TODO: move backwards
          _ -> Nothing
        "shoot" -> Just ShootPath direction -- TODO: shoot path
        -- TODO: build out shooting I/O
        "smell" -> Just Smell -- TODO: return sniffing action
        "feel" -> Just Feel -- TODO: return feeling action
        "listen" -> Just Listen -- TODO: return listening action
        _ -> Nothing

{-
TODO:
  - showState: returns data of curr game state
  - initialState: defines the init state of the game
  - applyAction: creates a modified game state with some action
  - isGameOver: bool, return if game is over or not
  - ShootPath: takes in directions to shoot the arrow, fires an arrow in some direction
-}

{-
Initialize game loop with starting game state
Loop through the game, each time asking the user for an input
Addressing the input, and recursing on the new game state
-}