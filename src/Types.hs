type Position = Int
data Move = Left | Right | Back

data Orientation = Position deriving (Eq) 


data GameState = GameState {
    playerPosition    :: Position,
    batPosition       :: Position,
    pitPosition       :: Position,
    wumpusPosition    :: Position,
    orientation       :: Orientation,
    wumpusIsHungry    :: Bool,
    playerArrowCount  :: Int
}


move :: Position -> Orientation -> Move -> Position
-- move 1 2 Main.Left = 8
-- move 1 8 Main.Left = 8
-- move 1 5 Main.Left = 8

move _ _ _ = undefined


