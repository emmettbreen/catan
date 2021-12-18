exception InvalidResource

val turn : Player.t -> Board.t -> Game.t -> bool -> unit
(** [turn pl b g flag] will take a turn for player [pl] given the board state 
[b] and game state [st]. Will roll if [flag = true], and will prompt [pl] with 
  options if [flag = false]*)