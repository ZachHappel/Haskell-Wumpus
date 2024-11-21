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



--heckWum
pus :: GameState -> Bool




