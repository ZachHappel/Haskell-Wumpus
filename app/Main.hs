module Main (main) where

import Lib
import Types
import System.Random(mkStdGen)

main :: IO()
main = do
    let gen = mkStdGen 27
    let (ws, newGen) = initializeWumpus gen
    let enviornment = initializeEnvironment newGen
    let player = initializePlayer 1
    print ws
    print enviornment
    print player



