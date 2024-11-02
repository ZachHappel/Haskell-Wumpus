# Hunt the Wumpus

_A collaborative implementation of Hunt the Wumpus in Haskell._

## Overview

_Hunt the Wumpus_ is a single-player, text-based game that pits an unfortunate hunter against a perilous, subterranean cave network that includes three threats: bottomless pits, colonies of "super bats", and the eponymous Wumpus (which is sleeps all the time). As a collaborative class project, we will design and implement a version of this classic game in Haskell using the functional programming paradigm.

The original game featured a completely dark network of `20` caves connected by tunnels in a configuration topologically equivalent to a dodecahedron, in which the vertices are caves and the edges are tunnels.

The Wumpus is unique within the cave network, but there are two instances each of the bottomless pits and the super-bats. Each threat results in a different outcome.
1. Upon entering a cave containing a bottomless pit, the hunter falls in and dies, as there is no escape from a bottomless pit.
2. Upon entering a cave containing a colony of super-bats, a swarm of bats picks up the hunter and drops her in a random cave. (Note that being dropped in a cave with another threat results the outcome associated with that threat.)
3. Upon entering a cave containing the sleeping Wumpus, the beast is startled and either (a) consumes the hunter whole, resulting in death, or (b) (a) flees to an adjacent cave.

Your instructor first encountered this game in a class on artificial intelligence while studying as a master's student in computer science. In that setting, the hunter is described as an "agent" possessing three senses for perceiving his otherwise completely dark cavern environment:
1. _Smell_ - The Wumpus itself exudes a foul stench that can be detected from nearby (i.e. connected) cave.
2. _Touch_ - Feeling a cool breeze indicates that a bottomless pit is nearby.
3. _Hearing_ - When a colony of super-bats is nearby, the faint sound of fluttering can be heard.

For more information about _Hunt the Wumpus_, please refer to the [Wikipedia article about the game](https://en.wikipedia.org/wiki/Hunt_the_Wumpus).
