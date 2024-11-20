module Types where

type Position = Int

-- if move is an enum then it forces the game to have
--    # of enums amount of connections per cave
--    a more generic way of representing moves could be
--    used but it may result it move names which are bland (e.i. to room 3)
--    or move names which are incorrect (e.i. moving left when in that position you can only move right)
-- For a decahedron it Left Right Back make sense for every move as you will always have those options
--    if orientated correctly
data Move = Left | Right | Back

data PlayerState = Player
  { playerPosition :: Position,
    -- will have to be set to the correct position on game start
    --    to orientate player
    lastPosition :: Position,
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

makeCircular :: (Ord a, Int a) => [a] -> [a]
makeCircular = fix $ ++

-- Map Layout:
decahedron :: CaveLayout
decahedron = -- [Left, Right, Back]
  [ (1, makeCircular [2, 5, 8]),
    (2, makeCircular [3, 1, 10]),
    (3, makeCircular [4, 2, 12]),
    (4, makeCircular [5, 3, 14]),
    (5, makeCircular [1, 4, 6]),
    (6, makeCircular [7, 15, 5]),
    (7, makeCircular [8, 6, 17]),
    (8, makeCircular [9, 7, 1]),
    (9, makeCircular [10, 8, 18]),
    (10, makeCircular [11, 9, 2]),
    (11, makeCircular [12, 10, 19]),
    (12, makeCircular [13, 11, 3]),
    (13, makeCircular [14, 12, 20]),
    (14, makeCircular [15, 13, 4]),
    (15, makeCircular [6, 14, 10]),
    (16, makeCircular [17, 20, 15]),
    (17, makeCircular [18, 16, 7]),
    (18, makeCircular [19, 17, 9]),
    (19, makeCircular [20, 18, 11]),
    (20, makeCircular [16, 19, 13])
  ]

-- CaveLayout -> Current Position -> Last Position -> Move -> Position
move :: CaveLayout -> Position -> Position -> Move -> Position
-- Example of how last position is helpful:
-- let current position be 1
-- let last position be 2
-- let 1's mapLayout entry be (1, [2, 5, 8]),
-- Move Back -> obviously return 2
-- Move Left -> move left of 2 (cyclically if out of bounds) return 8
-- Move Right -> move right of 2 return 5
move _ _ _ = undefined
