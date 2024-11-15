import System.Random ()
import Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type GameState = (PlayerState, WumpusState, EnvironmentState, gen)

f :: CaveLayout -> GameState -> GameState
f = undefined

x = do
  msg <- GameState
  putStrLn msg