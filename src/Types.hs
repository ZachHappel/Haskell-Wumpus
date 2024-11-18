module Types where

type Position = Int

-- if move is an enum then it forces the game to have
--    # of enums amount of connections per cave
--    a more generic way of representing moves could be
--    used but it may result it move names which are bland (e.i. to room 3)
--    or move names which are incorrect (e.i. moving left when in that postion you can only move right)
-- For a decahedron it Left Right Back make sense for every move as you will always have those options
--    if orientated correctly
data Move = Left | Right | Back

data PlayerState = Player
  { playerPosition :: Position,
    -- will have to be set to the correct position on game start
    --    to orientate player
    lastPostion :: Position,
    playerArrowCount :: Int
  }

data WumpusState = WumpusState
  { wumpusPosition :: Position
  }

data EnvironmentState = EnvironmentState
  { hazards :: [(Position, Hazard)]
  }

data Hazard = Bats | Pit

type CaveLayout = [[Position]]

