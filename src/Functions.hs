module Functions where
import Types

import Data.Maybe (mapMaybe)
import System.Random (randomRIO)

--movePlayer :: GameState -> String -> GameState


-- user says "shoot" then which direction, then get gamestate to see where wumpus is, then return True/False if wumpus dies
--shootArrow :: String -> String-> GameState -> Bool
--shootArrow = 


smell :: GameState -> String
smell game = 
  let playerPos = playerPosition $ playerState game
      env       = environment game
      wumpusNearby = wumpusLocation env `elem` getNeighbors playerPos (layout game)
  in if wumpusNearby
    then "You smell something foul nearby..."
    else "You smell nothing unusual."


listen :: GameState -> String
listen game = 
  let playerPos = playerPosition $ playerState game
      env   = environment game
      neighbors = getNeighbors playerPos (layout game)
      batSounds = any (`elem` batsLocations env) neighbors
      pitSounds = any (`elem` pitsLocations env) neighbors
      sounds =
        ["fluttering of wings" | batSounds ] ++ 
        ["whirring wind" | pitSounds]
  in if null sounds
      then "You hear nothing unusual."
      else "You hear: " ++ unwords sounds



shoot :: String -> GameState -> String
shoot direction game =
  let playerPos = playerPosition $ playerState game
      env       = environment game
      gameLayout    = layout game -- needed to call it gameLayout to get beyond naming conflict
      previous  = lastPostion $ playerState game
      neighbors = getOrientationAdjustedNeighbors playerPos previous gameLayout
      targetCave = case direction of
        "Back"  -> neighbors !! 0     
        "Right" -> neighbors !! 1
        "Left"  -> neighbors !! 2
        _       -> error "Invalid direction"
  in if targetCave == wumpusLocation env
       then "You killed the Wumpus! You win!"
       else "You missed! The Wumpus is still alive."

{-
smell :: GameState -> String
smell game = 
  let playerPos = playerPosition $ playerState game
      nearbyHazards = [hazard | (pos, hazard) <- hazards (environment game), ]
-}


-- Rewrote randomizer/random so that it more easily integrates with the data and type definitions we already have
randomizeEnvironment :: [Position] -> IO EnvironmentState
randomizeEnvironment roomIds = do
  wumpusRoom  <- randomPick roomIds
  batRooms    <- randomPicks roomIds [wumpusRoom] 2
  pitRooms    <- randomPicks roomIds (wumpusRoom : batRooms) 2
  return EnvironmentState
    {
      wumpusLocation = wumpusRoom,
      batsLocations = batRooms,
      pitsLocations = pitRooms
    }


randomPick :: [Position] -> IO Position
randomPick roomIds = do
  index <- randomRIO (0, length roomIds - 1)
  return (roomIds !! index)


randomPicks :: [Position] -> [Position] -> Int -> IO [Position]
randomPicks roomIds exclude n = do
  let available = filter (`notElem` exclude) roomIds
  if n > length available
    then error "Somehow not enough rooms"
    else mapM (const $ randomPick available) [1..n] -- what does mapM and const do here again? 




{-

assignRandomFeatures :: [RoomId] -> IO [(RoomId, RoomFeature)]
assignRandomFeatures roomIds = do
      wumpusRoom <- randomPick roomIds
      batRooms <- randomPicks roomIds [wumpusRoom] 2
      pitRooms <- randomPicks roomIds (wumpusRoom : batRooms) 2
      let hazards = [(wumpusRoom, Wumpus)]
                    ++ [(r, Bat) | r <- batRooms]
                    ++ [(r, Breeze) | r <- pitRooms]
      return hazards

randomPick :: [RoomId] -> IO RoomId
randomPick roomIds = do
  index <- randomRIO (0, length roomIds - 1)
  return (roomIds !! index)

randomPicks :: [RoomId] -> [RoomId] -> Int -> IO [RoomId]
randomPicks roomIds exclude n = do
  let available = filter (`notElem` exclude) roomIds
  if n > length available
    then error "Not enough available rooms for selection!"
    else do
      picked <- mapM (const $ randomPick available) [1 .. n]
      return picked

-}



{-

senseRoom :: Room -> String
senseRoom room =
  case roomFeature room of
    Wumpus -> "WUMPUS NEAR BY!"
    Bat    -> "Flutters"
    Breeze -> "Breeze!"
    Empty  -> "The room is empty."


  
-}



