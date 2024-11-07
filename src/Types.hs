type Arrows = Int

type RoomNumber = Int
type EventType = String

data Room = Room RoomNumber EventType
data Player = Player Arrows RoomNumber
data Wumpus = Wumpus RoomNumber
data Map = Map [Room]
