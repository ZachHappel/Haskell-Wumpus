module Main where
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
  case parseInput input of {} -- cases for what to do when the input is valid
    Just action -> do -- in the case the action is valid
      let newState = applyAction state action -- create the new game state with the curr state and action
      if isGameOver newState -- if this resulting game state exists and results in a death
        then putStrLn "Game over"
        else gameLoopIO newState -- else, continue game loop
    Nothing -> do -- in the case the action is invalid
      putStrLn "Invalid action"
      gameLoopIO state -- continue the game loop re-asking "Enter move"

parseInput :: String -> Maybe Action
parseInput input =
  let (cmd, dir) = map $ toLower (break isSpace input)
  case (cmd, dir) of
    ("move", dir) -> case dir of
      "left" -> -- TODO: move left
      "right" -> -- TODO: move right
      "back" -> -- TODO: move backwards
      _ -> Nothing
    ("shoot", path) -> case path of
      -- TODO: build out shooting I/O
      _ -> Nothing
    ("smell") -> -- TODO: return sniffing action
    ("feel") -> -- TODO: return feeling action
    ("listen") -> -- TODO: return listening action
    _ -> Nothing
  where
    isSpace = do
      -- TODO: Get all chars before space

{-
TODO:
  - showState: returns data of curr game state
  - initialState: defines the init state of the game
  - applyAction: creates a modified game state with some action
  - isGameOver: bool, return if game is over or not
-}

{-
Initialize game loop with starting game state
Loop through the game, each time asking the user for an input
Addressing the input, and recursing on the new game state
-}