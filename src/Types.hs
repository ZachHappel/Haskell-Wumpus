type Position = Int
data Move = Left | Right | Back


data GameState = GameState {
    playerPosition    :: Position,
    batPosition       :: Position,
    pitPosition       :: Position,
    wumpusPosition    :: Position,
    wumpusIsHungry    :: Bool,
    playerArrowCount  :: Int
}


move :: Position -> Move -> Move
move = undefined
