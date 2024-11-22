module Lib
  (
    someFunc,
    selectRandomElement,
    getRandomPosition,
    move,
    setState,
    handleMovement,
    handleShooting,
    testCaveLayout
  ) where

import qualified System.Random as Random
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Old Dodecahedron map to test with
testCaveLayout :: CaveLayout
testCaveLayout =
  [ (1,  [2, 5, 8]),
    (2,  [1, 3, 10]),
    (3,  [2, 4, 12]),
    (4,  [3, 5, 14]),
    (5,  [1, 4, 6]),
    (6,  [5, 7, 15]),
    (7,  [6, 8, 17]),
    (8,  [1, 7, 9]),
    (9,  [8, 10, 18]),
    (10, [2, 9, 11]),
    (11, [10, 12, 19]),
    (12, [3, 11, 13]),
    (13, [12, 14, 20]),
    (14, [4, 13, 15]),
    (15, [6, 14, 16]),
    (16, [15, 17, 20]),
    (17, [7, 16, 18]),
    (18, [9, 17, 19]),
    (19, [11, 18, 20]),
    (20, [13, 16, 19])
  ]

-- Moves the player, in main this would be called before setState
-- CaveLayout, currentPosition, previousPosition, noveDirection, new position
move :: CaveLayout -> Position -> Position -> Move -> Position
move layout currentPosition previousPosition moveType =
  case lookup currentPosition layout of
    Just connections ->
      case moveType of
        MoveBack -> previousPosition
        MoveLeft  -> cyclicMove connections lastIndex 1
        MoveRight -> cyclicMove connections lastIndex (-1)
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


-- Function that modifies the state of the game depending on the players actions
setState :: PlayerState -> Action -> GameState -> GameState
setState player action gameState =
  case action of
    MoveAction moveDir ->
      -- Existing movement logic
      handleMovement player moveDir gameState
    ShootAction path ->
      -- Shooting logic
      handleShooting player path gameState

-- Function to handle movement of player
-- Take in current player state, action they performed, curent game state, and gives new game state
handleMovement :: PlayerState -> Move -> GameState -> GameState
handleMovement player moveDir gameState =
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

handleShooting :: PlayerState -> [Position] -> GameState -> GameState
handleShooting player path gameState =
  let
    genVal = gen gameState
    wumpus = wumpusState gameState
    wumpusPos = wumpusPosition wumpus
    remainingArrows = playerArrowCount player - 1

    -- Check if the player has arrows left
    gameStatus' = if remainingArrows < 0
                  then GameOver "You have no arrows left!"
                  else gameStatus gameState

    -- Check if the arrow path exceeds the maximum length
    gameStatus'' = if length path > 5
                   then GameOver "Your arrow cannot travel more than 5 caves!"
                   else gameStatus'

    -- Validate the arrow path
    isValidPath = validateArrowPath (playerPosition player) path

    -- Update game status if path is invalid
    gameStatus''' = if not isValidPath
                    then GameOver "Invalid arrow path!"
                    else gameStatus''

    -- Determine if the arrow hits the Wumpus
    arrowHitsWumpus = wumpusPos `elem` path

    -- Update game status if the Wumpus is hit
    finalGameStatus = if arrowHitsWumpus
                      then GameOver "You have killed the Wumpus! You win!"
                      else gameStatus'''

    -- If the arrow misses and the game is ongoing, the Wumpus may move
    (updatedWumpusState, finalGen) = if not arrowHitsWumpus && finalGameStatus == Ongoing
                                     then moveWumpusRandomly wumpus genVal
                                     else (wumpus, genVal)

    -- Update the player's arrow count
    updatedPlayer = player { playerArrowCount = remainingArrows, playerHasShot = True }

  in
    gameState {
      playerState = updatedPlayer,
      wumpusState = updatedWumpusState,
      gen = finalGen,
      gameStatus = finalGameStatus
    }

validateArrowPath :: Position -> [Position] -> Bool
validateArrowPath _ [] = True  -- Empty path is valid
validateArrowPath currentPos (nextPos:rest) =
  case lookup currentPos testCaveLayout of
    Just connections ->
      (nextPos `elem` connections) && validateArrowPath nextPos rest
    Nothing -> False  -- Current position not found in the cave layout


-- Helper function that generates a random position, used for Bats and Wumpus moving
selectRandomElement :: Random.StdGen -> [a] -> (a, Random.StdGen)
selectRandomElement genValue list =
  let
    (idx, newStdGen) = Random.randomR (0, length list - 1) genValue
    element = list !! idx
  in
    (element, newStdGen)

-- Used for bats transporting player to a random cave
getRandomPosition :: Random.StdGen -> CaveLayout -> (Position, Random.StdGen)
getRandomPosition genValue cave =
  let
    allPositions = map fst cave
    (newPos, newStdGen) = selectRandomElement genValue allPositions
  in
    (newPos, newStdGen)

moveWumpusRandomly :: WumpusState -> Random.StdGen -> (WumpusState, Random.StdGen)
moveWumpusRandomly currentWumpusState genValue =
  let
    wumpusPos = wumpusPosition currentWumpusState
    connections = fromMaybe [] (lookup wumpusPos testCaveLayout)
    (newWumpusPos, newGen) = selectRandomElement genValue connections
    updatedWumpus = currentWumpusState { wumpusPosition = newWumpusPos }
  in
    (updatedWumpus, newGen)
