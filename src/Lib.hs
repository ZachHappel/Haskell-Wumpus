module Lib
  ( 
    someFunc,
    getRandomPosition,
    move,
    setState,
    testCaveLayout
  ) where

import qualified System.Random as Random
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Small cave to test with
testCaveLayout :: CaveLayout
testCaveLayout =
  [ (1, [2, 3, 4]),  -- Position 1 connects to 2, 3, 4
    (2, [1, 3, 4]),  -- Position 2 connects to 1, 3, 4
    (3, [1, 2, 4]),  -- Position 3 connects to 1, 2, 4
    (4, [1, 2, 3])   -- Position 4 connects to 1, 2, 3
  ]

-- Used for bats transporting player to a random cave
getRandomPosition :: Random.StdGen -> CaveLayout -> (Position, Random.StdGen)
getRandomPosition genValue cave =
  let allPositions = map fst cave
      (idx, newStdGen) = Random.randomR (0, length allPositions - 1) genValue :: (Int, Random.StdGen)
      newPos = allPositions !! idx
  in (newPos, newStdGen)

-- Moves the player, calls setState after every move
move :: CaveLayout -> Position -> Position -> Move -> Position
move layout currentPosition previousPosition moveType =
  case lookup currentPosition layout of
    Just connections ->
      case moveType of
        MoveBack -> previousPosition
        MoveLeft  -> cyclicMove connections lastIndex (-1)
        MoveRight -> cyclicMove connections lastIndex 1
      where
        -- Index of the last position in the connections list
        lastIndex = case elemIndex previousPosition connections of
                      Just idx -> idx
                      Nothing  -> error "Last position not found in connections"

        -- Helper function for cyclic movement
        cyclicMove :: [Position] -> Int -> Int -> Position
        cyclicMove conns currentIdx step =
          let newIdx = (currentIdx + step) `mod` length conns
          in conns !! newIdx
    Nothing -> error "Invalid position in cave layout"

-- Function to set a new game state after the player moves
setState :: PlayerState -> Move -> GameState -> GameState
setState player moveDir gameState =
  let
    -- Extract fields from GameState
    wumpus = wumpusState gameState
    env = environmentState gameState
    genVal = gen gameState
    --status = gameStatus gameState     This will be used once main and game is working

    currentPos = playerPosition player
    lastPos = lastPosition player

    hazardsList = hazards env
    wumpusPos = wumpusPosition wumpus

    -- Determine new position based on moveDir
    newPos = move testCaveLayout currentPos lastPos moveDir

    -- Update PlayerState
    updatedPlayer = player { 
      lastPosition = currentPos,
      playerPosition = newPos,
      playerHasShot = False     -- Reset shot status everytime the player moves
    }

    -- Check for hazards at the new position
    hazard = lookup newPos hazardsList

    -- Check if player has encountered the Wumpus
    wumpusEncounter = newPos == wumpusPos

    -- Handle hazard effects
    (interimPlayer, interimGen, interimStatus) = case hazard of
      Just Bats ->
        -- Transport player to a random position
        let (randPos, newStdGen) = getRandomPosition genVal testCaveLayout
            transportedPlayer = updatedPlayer { 
              playerPosition = randPos,
              lastPosition = newPos
            }
        in (transportedPlayer, newStdGen, Ongoing)
      Just Pit ->
        -- Player falls into a pit, game over
        (updatedPlayer, genVal, GameOver "You fell into a pit!")
      Nothing ->
        -- No hazard encountered
        (updatedPlayer, genVal, Ongoing)

    -- Handle Wumpus encounter with random chance
    (finalPlayer, finalWumpus, finalGen, finalStatus) = if wumpusEncounter
      then
        let (chance, newStdGen) = Random.randomR (1, 2) interimGen :: (Int, Random.StdGen) -- 1: Player dies, 2: Wumpus flees
        in if chance == 1
           then
             -- Player dies
             (interimPlayer, wumpus, newStdGen, GameOver "You were eaten by the Wumpus!")
           else
             -- Wumpus flees to a random adjacent cave
             let connections = fromMaybe [] (lookup wumpusPos testCaveLayout)
                 (idx, updatedGen') = Random.randomR (0, length connections - 1) newStdGen :: (Int, Random.StdGen)
                 newWumpusPos = connections !! idx
                 newWumpus = wumpus { wumpusPosition = newWumpusPos }
             in (interimPlayer, newWumpus, updatedGen', Ongoing)
      else
        -- No Wumpus encounter
        (interimPlayer, wumpus, interimGen, interimStatus)

    -- Determine the final game status
    finalGameStatus = finalStatus
  in
    -- Return the updated GameState
    gameState { 
      playerState = finalPlayer,
      wumpusState = finalWumpus,
      gen = finalGen,
      gameStatus = finalGameStatus
    }
      
--x = do
--  msg <- GameState
--  putStrLn msg
