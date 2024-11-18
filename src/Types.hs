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
    playerArrowCount :: Int,
    playerHasShot :: Boolean
  }

data WumpusState = WumpusState
  { wumpusPosition :: Position
  }

data EnvironmentState = EnvironmentState
  { hazards :: [(Position, Hazard)]
  }

data Hazard = Bats | Pit

type CaveLayout = [(Position, [Position])]

-- Map Layout:
decahedron :: CaveLayout
decahedron =
  [ (1, [2, 5, 8]),
    (2, [1, 3, 10]),
    (3, [2, 4, 12]),
    (4, [3, 5, 14]),
    (5, [1, 4, 6]),
    (6, [5, 7, 15]),
    (7, [6, 8, 17]),
    (8, [1, 7, 9]),
    (9, [8, 10, 18]),
    (10, [2, 9, 11]),
    (11, [10, 12, 20]),
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

-- CaveLayout -> Current Position -> Last Position -> Move -> Position
move :: CaveLayout -> Position -> Position -> Move -> Position
-- Example of how last postion is helpful:
-- let current position be 1
-- let last position be 2
-- let 1's mapLayout entry be (1, [2, 5, 8]),
-- Move Back -> obivously return 2
-- Move Left -> move left of 2 (cyclically if out of bounds) return 8
-- Move Right -> move right of 2 return 5
move _ _ _ = undefined
