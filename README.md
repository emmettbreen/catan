# `Catan` 
A terminal-based version of Catan written in OCaml for my CS 3110 final project.

My contributions include:

- board.ml: a purely functional backend module that can
  - randomly generate catan maps
  - determine valid/invalid placement of settlements/cities/roads
  - keep track of where each player has settlements/cities/roads
  - block resource collection on specified regions of the board
  - distribute resources to players based on a given roll number
- input.ml: a purely functional game engine that
  - uses strictly recursion to change game state based on user input
  - reacts to user inputs, handles exceptions, and interprets information
  - displays the board, resources, and options each turn
- 



## Authors
- Emmett Breen
- Evan Williams
- Jack Li
- Zach Ross
