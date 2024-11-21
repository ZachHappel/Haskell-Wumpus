module Main where

import Data.Char (toLower)
import Data.List (break)

-- import Lib
-- import GameLogic (Action, GameState, gameLoop, initialState, isGameOver) TODO: Example parameters for gameLogic function

main :: IO ()
main = do
  putStrLn "Welcome Hunt the Wumpus!"
  rules
  putStrLn "To see the rules at any time just type 'rules'."
  gameLoopIO initialState -- Begin game loop with init state

gameLoopIO :: GameState -> IO ()
gameLoopIO state = do
  putStrLn $ "You're inside cave #" ++ showState state -- Display the cave #n the player is in
  putStrLn "Perform an action:"
  input <- getLine
  case parseInput input of -- cases for what to do when the input is valid or not
    Just action -> do
      output <- parseActionOutput action
      putStrLn output
      -- in the case the action is valid
      let (newState, event) = applyAction state action -- create the new game state with the curr state and action
      eventOut <- parseGameOutput event
      putStrLn eventOut
      if isGameOver newState -- if this resulting game state exists and results in a death
        then putStrLn "Game over!"
        else gameLoopIO newState -- else, continue game loop
    Nothing -> do
      if input == "rules"
        then do
          rules
          gameLoopIO state -- continue the game loop after displaying the rules
        else do
          putStrLn "Invalid action"
          gameLoopIO state -- continue the game loop re-asking "Enter move"

parseInput :: String -> Maybe Action
parseInput input =
  -- break the input into lowercase words to be further distinguished
  case words $ map toLower input of
    "move" : dir : _ -> case dir of
      "left" -> Just Move left
      "right" -> Just Move right
      "back" -> Just Move back
      _ -> Nothing
    "shoot" : directions -> Just $ ShootPath directions
    "smell" -> Just Smell
    "feel" -> Just Feel
    "listen" -> Just Listen
    _ -> Nothing

parseActionOutput :: Action -> IO String
parseActionOutput action =
  return $ case action of
    Move left -> "You move into the cave to your left."
    Move right -> "You move into the cave to your right."
    Move back -> "You move back into the prior cave."
    ShootPath directions -> "You shoot an arrow following the path: " ++ unwords directions
    Smell -> "You sniff the air for the sour smell of the Wumpus."
    Feel -> "You touch the floor and walls of the cave, you feel for any breeze."
    Listen -> "You hold your breath and listen for any sounds in the cave."

parseGameOutput :: GameEvent -> IO String
parseGameOutput event =
  return $ case event of
    WumpusDied -> "You hear a shriek and large thud as the Wumpus falls to the floor, defeated."
    FellIntoPit -> "You slip and fall into the bottomless pit, falling for your remaining days."
    MovedByBats -> "You're carried to another cave by a swarm of cave bats."
    ArrowMissed -> "Your arrow clatters to the ground, no other noises are made in the dark."
    ArrowStartled -> "Your arrow clatters to the ground, the Wumpus is heard scurrying away."
    PlayerKilled -> "The Wumpus chases you down and with its giant mouth, devours you whole."

rules :: IO ()
rules = do
  putStrLn "-- Rules --"
  putStrLn ""
  putStrLn "1. The goal of the game is to hunt and kill the dreaded Wumpus by:"
  putStrLn "      Shooting the Wumpus with an arrow has a 50/50 chance to kill it or scare it"
  putStrLn "      If the Wumpus flees it runs to a neighboring room and goes back to sleep"
  putStrLn "      Using 'Sense Smell' allows the player to sense if the Wumpus is in a room neighboring the current cave"
  putStrLn ""
  putStrLn "2. How to play:"
  putStrLn "      Using your wits and ingenuity, you must hunt and kill the deadly Wumpus."
  putStrLn "      In each cave, activate your senses 'Sense <sense>' and gather data on your environment."
  putStrLn "      Fire magical Crooked Arrows using 'Shoot <distance>' through (max. 5) caves in an attempt to kill the Wumpus, or scare it."
  putStrLn "      Move to an adjacent cave 'Move <direction>' using Left/Right/Back in respect to how the player enters the current cave."
  putStrLn ""
  putStrLn "3. In each cave the player can either:"
  putStrLn "      Move <direction> - to another cave (Left, Right, Back)"
  putStrLn "      Sense <sense> - smell, hear, feel"
  putStrLn "      Shoot <distance> - path arrow takes (Left/Right/Left/Right)"
  putStrLn ""
  putStrLn "4. In-game Situations: The player..."
  putStrLn "      enters into the room with the Wumpus, 50/50 chance for Wumpus to flee or kill you"
  putStrLn "      enters a room with Cave Bats, they're transported to a random cave"
  putStrLn "      enters a room with a bottomless pit, they fall in and die"
  putStrLn "      uses a sense and it records information from neighboring caves"
  putStrLn "      fires an arrow some distance through the caves, if the Wumpus is in the destination cave, 50/50 chance to startle it or kill it"

{-
TODO:
  - showState: returns data of curr game state
  - initialState: defines the init state of the game
  - applyAction: creates a modified game state with some action
  - isGameOver: bool, return if game is over or not
  - ShootPath: takes in directions to shoot the arrow, fires an arrow in some direction

Initialize game loop with starting game state
Loop through the game, each time asking the user for an input
Addressing the input, and recursing on the new game state
-}