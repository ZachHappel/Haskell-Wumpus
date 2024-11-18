import System.Random ()
import Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type GameState = (PlayerState, WumpusState, EnvironmentState, gen)

f :: CaveLayout -> GameState -> GameState
f = undefined

-- Take in players current state, current GameState, and generate a new GameState
setState :: PlayerState -> GameState -> GameState
setState player gameState = 
  let
    -- How player states would be accessed
    currentPos = playerPosition player
    lastPos = lastPostion player
    arrowShot = playerHasShot player

    -- How game states would be accessed
    hazardsList = hazards (environmentState gameState)
    wumpusPos = wumpusPosition (wumpusState gameState)
    
    -- Check for hazards at the current position
    hazard = lookup currentPos hazardsList

    -- Check if player has encountered the Wumpus
    wumpusEncounter = currentPos == wumpusPos


move :: CaveLayout -> Position -> Position -> Move -> Position
move layout currentPosition lastPosition moveType =
  case lookup currentPosition layout of
    Just connections ->
      case moveType of
        Back -> lastPosition
        Left -> cyclicMove connections lastIndex (-1)
        Right -> cyclicMove connections lastIndex 1
    Nothing -> error "Invalid position in cave layout"
  where
    -- Index of the last position in the connections list
    lastIndex = case elemIndex lastPosition connections of
                  Just idx -> idx
                  Nothing -> error "Last position not found in connections"

    -- Helper function for cyclic movement
    cyclicMove :: [Position] -> Int -> Int -> Position
    cyclicMove conns currentIndex step =
      let newIndex = (currentIndex + step) `mod` length conns
      in conns !! newIndex
      
x = do
  msg <- GameState
  putStrLn msg

