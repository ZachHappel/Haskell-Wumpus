-- take a list of displayable then return one that user selection



-- getFromUser :: Show a => String -> [a] -> a
-- getFromUser = undefined


-- getFromUser "Select the move:" [Left, Back]

{-
Select the move:
1 Left
2 Back
Left
invalid choice
Select the move:
1 Left
2 Back
-}


-- enter a room, do an action (sense shoot or nothing), then you get prompted to move
-- arrow movement needs to be implemented


getMoveFromUser :: String -> String
getMoveFromUser move 
    putStrLn "Enter your move (1 for Left, 2 for Right, 3 for Back): "
    | move == "1" = "Left"
    | move == "2" = "Right"
    | move == "3" = "Back"
    | otherwise = do
        putStrLn $ "Invalid Option: put another move"
        getMoveFromUser -- call this again


main :: IO ()
main = do  
    let result = getMoveFromUser move
    putStrLn $ "You chose: " ++ result


-- Function to get the user's move with validation
-- getMoveFromUser :: IO String
-- getMoveFromUser = do
--     putStrLn "Enter your move (1 for Left, 2 for Right, 3 for Back): "
--     move <- getLine  -- Read user input
--     case move of
--         "1" -> return "Left"
--         "2" -> return "Right"
--         "3" -> return "Back"
--         _ -> do
--             putStrLn "Invalid Option: Please try again."
--             getMoveFromUser  -- Recursively call until valid input is received

-- -- Main function
-- main :: IO ()
-- main = do
--     result <- getMoveFromUser  -- Get the validated move
--     putStrLn $ "You chose: " ++ result