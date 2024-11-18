import System.Random ()
import Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type GameState = (PlayerState, WumpusState, EnvironmentState, gen)

f :: CaveLayout -> GameState -> GameState
f = undefined
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

