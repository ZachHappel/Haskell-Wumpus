<!-- PLEASE ADD/MODIFY (TO) THIS FILE IF ANY OTHER MODIFICATIONS SHOULD BE MADE, FOR ANY MEMBER ON ANY TASK. -->
<!-- If a member does modify this file please let other members know. -->

# Divisions of Labor: Team Haskell

## Member 1: Map Layout and Cave Randomization

- **Tasks**:
  1. Finalize the map generation logic:
     - Ensure the cave network is represented, with each cave uniquely identified.
     - Implement the dodecahedron-like structure, ensuring 20 caves are connected correctly via their L,R,Back neighbors.
     - Randomly assign contents to caves:
       - 1 Wumpus.
       - 2 bottomless pits.
       - 2 hoards of super-bats.
       - Player starting position (be sure not to start them in a room with an entity in it).
  2. Be sure to generate the "sense data" (smell, touch, hearing) for each cave based on its surrounding neighbors.

---

## Member 2: Game Logic and Core Gameplay (Luciano)

- **Tasks**:
  1. Implement movement and interaction logic:
     - Allow the player to move between connected caves.
       - Handle cave orientation on a move.
     - Handle interactions based on cave contents:
       - Pit: End game (death).
       - Wumpus: Random chance of death or Wumpus fleeing to an adjacent cave.
       - Bats: Move the player to a random cave.
  2. Add shooting mechanics:
     - Allow the player to shoot arrows into connected caves.
     - Determine if the arrow hits the Wumpus and end the game (victory).
     - Decrement the player's arrow count after each shot.
     - Determine the number of caves the arrow travels.
  3. Implement a function to update the game state after each action.
  4. Write functions to check for win/loss conditions.

---

## Member 3: I/O and Game Integration (Connor)

- **Tasks**:
  1. Build a text-based user interface:
     - Display the current room description, warnings (e.g., smell, breeze, fluttering), and player stats (arrows left) ONLY IF the player requests the sense or their stats.
     - Parse input for player actions (move, shoot, etc.) when the user specifies.
  2. Handle user input:
     - Parse and validate player commands.
     - Connect inputs to the appropriate game logic functions.
  3. Integrate all components:
     - Combine map generation, game logic, and I/O into a cohesive gameplay loop.
     - Ensure the game operates correctly from start to finish, with a clear win/loss outcome.
