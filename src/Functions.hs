module Functions where
import Types

import Data.Maybe (mapMaybe)
import System.Random (randomRIO)

--movePlayer :: GameState -> String -> GameState

senseRoom :: Room -> String
senseRoom room =
  case roomFeature room of
    Wumpus -> "WUMPUS NEAR BY!"
    Bat    -> "Flutters"
    Breeze -> "Breeze!"
    Empty  -> "The room is empty."




assignFeatures :: CaveLayout -> IO [Room]
assignFeatures layout = do
  let roomIds = map fst layout
  wumpusRoom <- randomPick roomIds
  batRooms <- randomPicks roomIds [wumpusRoom] 2
  breezeRooms <- randomPicks roomIds (wumpusRoom : batRooms) 2

  let features = [(rid, Wumpus) | rid <- [wumpusRoom]] ++
                 [(rid, Bat) | rid <- batRooms] ++
                 [(rid, Breeze) | rid <- breezeRooms]
      rooms = map (\rid -> Room rid (lookupFeature rid features)) roomIds
  return rooms



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





--heckWum
pus :: GameState -> Bool




