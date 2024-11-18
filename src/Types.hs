module Types where

type Position = Int 
type Previous = Position
type Current = Position

-- if move is an enum then it forces the game to have
--    # of enums amount of connections per cave
--    a more generic way of representing moves could be
--    used but it may result it move names which are bland (e.i. to room 3)
--    or move names which are incorrect (e.i. moving left when in that postion you can only move right)
-- For a decahedron it Left Right Back make sense for every move as you will always have those options
--    if orientated correctly
data Move = Left | Right | Back deriving (Show)

data PlayerState = Player
  { playerPosition :: Position,
    -- will have to be set to the correct position on game start
    --    to orientate player
    lastPostion :: Position,
    playerArrowCount :: Int
  } deriving (Show)

data WumpusState = WumpusState
  { wumpusPosition :: Position
  } deriving (Show)

data EnvironmentState = EnvironmentState
  { hazards :: [(Position, Hazard)]
  } deriving (Show)


data GameState = GameState
  { playerState :: PlayerState,
    wumpusState :: WumpusState,
    environment :: EnvironmentState,
    layout      :: CaveLayout
  } deriving (Show)


data Hazard = Bats | Pit deriving (Show)

type CaveLayout = [(Position, [Position])] 

-- Map Layout:
decahedron :: CaveLayout
decahedron =
  [ (1, [2, 5, 8, 2, 5, 8]),
    (2, [1, 3, 10, 1, 3, 10]),
    (3, [2, 4, 12, 2, 4, 12]),
    (4, [3, 5, 14, 3, 5, 14]),
    (5, [1, 4, 6, 1, 4, 6]),
    (6, [5, 7, 15, 5, 7, 15]),
    (7, [6, 8, 17, 6, 8, 17]),
    (8, [1, 7, 9, 1, 7, 9]),
    (9, [8, 10, 18, 8, 10, 18]),
    (10, [2, 9, 11, 2, 9, 11]),
    (11, [10, 12, 20, 10, 12, 20]), -- ????????????
    (12, [3, 11, 13, 3, 11, 13]),
    (13, [12, 14, 20, 12, 14, 20]),
    (14, [4, 13, 15, 4, 13, 15]),
    (15, [6, 14, 16, 6, 14, 16]),
    (16, [15, 17, 20, 15, 17, 20]),
    (17, [7, 16, 18, 7, 16, 18]),
    (18, [9, 17, 19, 9, 17, 19]),
    (19, [11, 18, 20, 11, 18, 20]),
    (20, [13, 16, 19, 13, 16, 19])
  ]



getNeighbors :: Position -> CaveLayout -> [Position]
getNeighbors current_position l = head [neighbors | (pos, neighbors) <- l, pos == current_position]

findIndexOf :: Eq a => a -> [a] -> Position
findIndexOf x xs = go xs 0
  where
    go [] _ = error "Not found"
    go (y:ys) i
      | x == y  = i
      | otherwise = go ys (i + 1)



-- e.g., getThreeIndicesStartingAtIndex 2 [2,5,8,2,5,8] => [8, 2, 5]
getThreeIndicesStartingAtIndex :: Int -> [Position] -> [Position]
getThreeIndicesStartingAtIndex index list = take 3 (drop index list) 

getOrientationAdjustedNeighbors :: Current -> Previous -> CaveLayout -> [Position] 
getOrientationAdjustedNeighbors current prev l = getThreeIndicesStartingAtIndex (findIndexOf prev (getNeighbors current l)) (getNeighbors current l)

-- takes index of the first occurrence of the player's "last position"
-- returns three indices with the last position being the first, followed by the right and the left
-- it does this by dropping up until the index of the first occurrence and returns the three init indices



caveArt :: String
caveArt = unlines
  [
  "*%@%%%%%@@@@@@@@*+%@@@@@@@@@@@@%%##%%%#%%##%#%%%%%@@%%%@@@@%@@@@@%%@@@@@@@@%#"
  ,"%%%%##%%@@@@@%##*#@@@@@@@@@@@@@@%%%%%#%@%@%%%#%%%%%%%%@@@@@@@%@@@@@@@@@@%@@%%"
  ,"+*#%#*###=#%@@%%@@@%@@@@@@@@@@@@@@@@@@%%%%@%%@@@@%+=*@@@@@@@@@@@@@@@%@@@@%%#%"
  ,"+###*#@#####@@@@@@@@@@@@@@@@@@@@@@@@@%@@@@@@@@@@@@%*%@@@@@@@@@@@@@@@@@@@@%%##"
  ,"#######%%%%%%@@@@@@@@@@@@@@@@@@@@@@@%%%%%%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@%%%%"
  ,"%%%#%%%%%%#%@@@@@@@@@@@@@%%@@@@@@@%%#%@%@@@@@@@@@@@@%@@@@@@@%##+%@@@@@@@%@@%%"
  ,"#%%%%%%%%%%%@@@@@@@@@@@@%%%%%%%%%%%%%@@@@@@@@@@@@@@@@@@@@%%*++=++=+*%@@@@@@@%"
  ,"##%%%%%%%%@@@@@%@@@@@%%%@%%#%%%%%%#%%@@@@@@@@@@@%%%%%@@@#=:.. ...-=-=+*%@@%%%"
  ,"#%@%%%%%%%@@%%%%%%%#+-:::::-*##%#%@@@@@@@@@%@%%%%%%%%#+:........  ...:-*@@@%@"
  ,"@%%#%%%@%@#+**#++=-:.........::*#%@@@@@@@@@@%#%%##%**=........        .:+%%%%"
  ,"#**+*##%@%***+=:..... .. ......-###@@@%@@@%@#*%##%**=.......            .=%%%"
  ,"%#######%%##+:         .........-#%@@@@%@@%%######*+.....                .=*+"
  ,"%%@#*%@%%%*#-.           ........+%@%@@@%@%@##%%%#*-..                    .*+"
  ,"%%%@@%@@@%#+.        ............:*@%@@@@@%%%%%##*-..                     .+#"
  ,"%%#%%@@@@@%=.         ...........:+#%@@@@@%@%@#%##:..                     .*%"
  ,"%####%@@%##-               .    ..=*%@@@@@@@%@%%@*....                    .#@"
  ,"%%%@@@@@@@*:                     .=*%@@@@@%@%@@%@=....                    -@%"
  ,"@@@@@%%%@@=.                 . . .=*%@@@@@@@@@@@@-...                    .+@%"
  ,"@@@@@@%%%%:.               .. ....-%@@@@@@@@@@@@%:::..                   .%@@"
  ,"%@@##+*%@%.               ........:*@@@@@@@@@@@@=.:...                   -@@@"
  ,"%###**##%%=.              .........=@@@@@@@@@%%+..  ..                   *@@@"
  ,"%%%%%#####*:                .....  .:*#%@@@@@#--:.                      .@@@@"
  ,"%%%%#*%%%%+:                ......::+@@@@@@@@@@#=-.                     :@@@@"
  ,"@@%%#==#%%=:.:--===----=++=--::.---%%@@@@@@@@@@@*=.                     +@@@@"
  ,"@@%+==++##-.:==++++********####*###@@@@@@@@@@@@@@+.....::::.......   ...#@@@@"
  ,"@@#+-=++*:.  .-==+++*********#####%%@@@@@@@@@@@@@%=-----==----:......:-:=@@@@"
  ,"%#**+*#%-=++*.:-=+++++******#########@@@@@@@@@@@@*+*####*****+====+*+=-=*@@@@"
  ,"@@%@@@@@+@@@@@:-==++++*******#####**#%@@@@@@@@@@%####%####*****++++====+@@@@@"
  ,"#@@@@@@@@@@@@*#***#####+++************%@@@@@@@@@######*********#**+**##%@@@@@"
  ,"%@@@@@@@@@@@@%%*####+=+++***********##%@@@@@@@@###**#******###*******##@@@@@@"
  ,"@@@@@@%%@@@@#==-=-::-+#%%%%#**********#@@@@@@@@%%##%%%%%#***********##@@@@@@@"
  ,"#%%@@@%%@@@@==+**#########*+++++++****##@@@@@#####**%###**********###@@@@@@@@"
  ,"*##%%@@@@@#+++++*##%%*+++**+**+++++++++**#%#*#####*#**##*#*#*****###%@@@@@@@@"
  ]



{-
-- CaveLayout -> Current Position -> Last Position -> Move -> Position
move :: CaveLayout -> Position -> Position -> Move -> Position
-- Example of how last postion is helpful:
-- let current position be 1
-- let last position be 2
-- let 1's mapLayout entry be (1, [2, 5, 8]),
-- Move Back -> obivously return 2
-- Move Left -> move left of 2 (cyclically if out of bounds) return 8
-- Move Right -> move right of 2 return 5

-- move _ _ _ _ = undefined
-- move layout current last Back = last
-- move layout current last Right = 
-- move layout current last Left = 

-- neighbors list comprehension returns ns, retrieved from layout's (position, [positions]) tuple
-- where pos == (value passed as `current`), as per the predicate 

-- also, neighbors uses `head` bc the list comprehension returns [[Position]] 
-- `head` allows us to retrieve the [Position]


move layout current last direction =
  let neighbors = head [ns | (pos, ns) <- layout, pos == current]
  in case direction of
       Back  -> last -- Value passed to the function
       Left  -> neighbors !! 0  -- Where first elem is left
       Right -> neighbors !! 1  -- Where secon elem is right
       Forward -> neighbors !! 2 -- Where third elem is forward
-}