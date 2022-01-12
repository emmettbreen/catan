# Catan
## 2021
A terminal-based version of Catan written in OCaml for my CS 3110 final project.

My contributions include:

- board.ml: a purely functional backend module that can
  - support any randomly generated catan map
  - determine valid/invalid placement of settlements/cities/roads
  - keep track of where each player has settlements/cities/roads
  - block resource collection on specified regions of the board
  - distribute resources to players based on a given roll number
- input.ml: a purely functional game engine that
  - uses strictly recursion to change game state based on user input
  - reacts to user inputs, handles exceptions, and interprets information
  - displays the board, resources, and options each turn
- main.ml: a module that handles
  - collection of preliminary information and game initialization such as
   - board set up (can be default or random)
   - number of players and their names
  - running the first two special turns (free settlement and road for each player)
  - beginning the recursive turn function and starting the actual game

## Installation Instructions
- install homebrew
- install gpatch and opam on homebrew
- install dune and ANSITerminal on opam
- use command "make play" to run and "make test" to test

## Authors
- Emmett Breen
- Evan Williams
- Jack Li
- Zach Ross


## Screenshots
<img src ="https://user-images.githubusercontent.com/90010213/146633149-60218925-de62-4b3c-bc1d-e4f1079187bd.png" width = "400" height = "600"/>  <img src="https://user-images.githubusercontent.com/90010213/146633157-9b205d0c-2070-4722-b940-420109438be0.png" width = "400" height = "600"/>

