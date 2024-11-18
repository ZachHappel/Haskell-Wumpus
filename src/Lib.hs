module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

initalizeHazards :: StdGen -> [(Position, Hazard)]
initalizeHazards g = 
    [(p1, Pit), (p2, Pit), (b1, Bats), (b2, Bats)]
    where [p1, p2, b1, b2] = take 4 $ nub $ map (`mod` 20) $ randoms g

initalizeState :: StdGen -> GameState
initalizeState g = 
    GameState {
        playerPosition = p,
        lastPosition = 0,
        playerArrowCount = 3,
        wumpusPosition = w,
        hazards = initalizeHazards()
    }
    where [p, w] = take 2 $ nub $ map (`mod` 20) $ randoms g