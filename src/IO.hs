import Types (Move(..))
-- enter a room, do an action (sense shoot or nothing), then you get prompted to move
-- arrow movement needs to be implemented

-- Function to get the user's move with validation
getMoveFromUser :: IO Move
getMoveFromUser = do
    putStrLn "Enter your move (1 for Left, 2 for Right, 3 for Back): "
    move <- getLine  -- Read user input
    case playerMoveToGame move of
        Just validMove -> do
            putStrLn $ "You selected: " ++ show validMove
            return validMove
        Nothing -> do
            putStrLn "Invalid Option: Please try again."
            getMoveFromUser  -- Recursively call until valid input is received

-- Function to convert user input string to the Move type
playerMoveToGame :: String -> Maybe Move
playerMoveToGame move = case move of
    "1" -> Just Types.Left
    "2" -> Just Types.Right
    "3" -> Just Types.Back
    _   -> Nothing

-- have the player input the direction of the arrow move
getArrowMoveFromUser :: IO Move
getArrowMoveFromUser = do
    putStrLn "Enter your move (1 for Left, 2 for Right): "
    move <- getLine  -- Read user input
    case playerMoveToGame move of
        Just validMove -> do
            putStrLn $ "You selected: " ++ show validMove
            return validMove
        Nothing -> do
            putStrLn "Invalid Option: Please try again."
            getMoveFromUser  -- Recursively call until valid input is received

-- Function to convert user input string for arrow to the Move type
arrowMoveToGame :: String -> Maybe Move
arrowMoveToGame move = case move of
    "1" -> Just Types.Left
    "2" -> Just Types.Right
    _   -> Nothing