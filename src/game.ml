open Printers

exception InvalidPlayer

exception InvalidTrade

type t = {
  board : Board.t;
  mutable players : Player.t array;
  mutable curr_player : int;
  mutable roads_available : int;
  mutable settlements_available : int;
  longest_road : Player.t option;
  largest_army : Player.t option;
}

let calculate_player_index (state : t) (pl_id : Player.player_id) =
  let rec get counter =
    if counter >= Array.length state.players then raise InvalidPlayer
    else if Player.get_player_id state.players.(counter) = pl_id then
      counter
    else get (counter - 1)
  in
  get (Array.length state.players - 1)

let rec generate_player_color state player =
  let index =
    calculate_player_index state (Player.get_player_id player)
  in
  get_color_from_index index |> Player.set_color player

let init_players_colors state (players : Player.t array) =
  Array.map (fun pl -> generate_player_color state pl) players

let init_game players board =
  let game =
    {
      board;
      players;
      curr_player = 0;
      roads_available = 70 - (2 * Array.length players);
      settlements_available = 53 - (2 * Array.length players);
      longest_road = None;
      largest_army = None;
    }
  in
  { game with players = init_players_colors game players }

let get_player_color_pairs game =
  List.map
    (fun pl -> (Player.get_player_id pl, Player.get_color pl))
    (Array.to_list game.players)

let set_curr_pl_ind num game = { game with curr_player = num }

let get_board gm = gm.board

let get_players gm = gm.players

let valid_player id gm =
  let valid = ref false in
  for i = 0 to Array.length gm.players do
    if Player.get_player_id gm.players.(i) = id then valid := true
    else ()
  done;
  !valid

let get_current_player gm = gm.players.(gm.curr_player)

let get_curr_pl_ind gm = gm.curr_player

let set_curr_player gm pl = gm.players.(gm.curr_player) <- pl

let set_players gm players = gm.players <- players

(*let get_player_roads gm pl = Board.get_player_edges_from_board pl
  gm.board*)

let next_player gm =
  let player_amt = Array.length gm.players in
  gm.curr_player <- (gm.curr_player + 1) mod player_amt

(*[get_cs_points_from_node player board index] is the number of victory
  points a player [player] has from cities and settlements on board
  [board] at the node with node_position [index]*)
let get_cs_points_from_node player board index =
  if Board.node_is_owned board index = false then 0
  else if Board.owner_of_node board index != Player.get_player_id player
  then 0
  else
    match Board.get_node_settlement board index with
    | Board.City -> 2
    | Board.Settlement -> 1

(*[vp_cs p b max_node_index] is the number of victory points [p] has on
  [b]. The function calls itself recurively [max_node_index] times. The
  initial [max_node_index] value is the number of nodes on the board -
  1. The standard value for this is 53. [vp] is the accumulated number
  of victory points from previous recurive calls.*)
let rec vp_cs p b max_node_index vp =
  if max_node_index < 0 then vp
  else
    vp_cs p b (max_node_index - 1)
      (get_cs_points_from_node p b max_node_index + vp)

(*[vp_from_cities_and_settlements p b] is the number of victory points
  [p] has on board [b] of the standard Catan board size*)
let vp_from_cities_and_settlements p b = vp_cs p b 53 0

let get_victory_points pl brd gm =
  vp_from_cities_and_settlements pl brd
  +
  if gm.longest_road = Some pl then 2
  else 0 + if gm.largest_army = Some pl then 2 else 0

let player_string pl brd gm =
  Player.to_string pl ^ "\n\tVictory Points: "
  ^ string_of_int (get_victory_points pl brd gm)

let update_board game new_board = { game with board = new_board }

let update_players game new_players =
  { game with players = new_players }

let get_player_index (state : t) (pl_id : Player.player_id) =
  let rec get counter =
    if counter >= Array.length state.players then raise InvalidPlayer
    else if Player.get_player_id state.players.(counter) = pl_id then
      counter
    else get (counter - 1)
  in
  get (Array.length state.players)

let get_player (state : t) (pl_id : Player.player_id) =
  state.players.(get_player_index state pl_id)

(* checks whether a player can give up the following resources*)
let rec check_pl_inv pl = function
  | [] -> ()
  | (r, n) :: t ->
      if Player.get_num_resource pl r < n then raise InvalidTrade
      else check_pl_inv pl t

let rec can_trade (pl : Player.t) (lst : (Player.resource * int) list) :
    unit =
  match lst with
  | [] -> ()
  | (r, n) :: t ->
      if Player.get_num_resource pl r < n then raise InvalidTrade
      else can_trade pl t

let get_pl_indx (state : t) (pl : Player.player_id) : int =
  let rec get (counter : int) =
    if Player.get_player_id state.players.(counter) = pl then counter
    else get (counter - 1)
  in
  get (Array.length state.players)

let trade
    (approver : Player.player_id)
    (wishlist : (Player.resource * int) list)
    (givelist : (Player.resource * int) list)
    (state : t) : unit =
  let p1 = state.players.(state.curr_player) in
  let p2 = get_player state approver in
  check_pl_inv p1 givelist;
  check_pl_inv p2 wishlist;
  state.players.(state.curr_player) <- Player.add_resources p1 wishlist;
  state.players.(get_player_index state approver) <-
    Player.add_resources p2 givelist

let rec list_max lst =
  match lst with
  | [] -> None
  | h :: t -> (
      let max_of_rest = list_max t in
      match max_of_rest with
      | None -> Some h
      | Some i -> Some (max i h))

let get_player_with_longest_road gm = failwith "Unimplemented"

let update_ind_player_in_game g p newP =
  Array.set g.players g.curr_player newP

let set_player g p newP =
  let () = update_ind_player_in_game g p newP in
  g

let get_game_with_incremented_player g =
  let () = next_player g in
  g

let rec build_list players board game =
  match players with
  | [] -> []
  | h :: t -> get_victory_points h board game :: build_list t board game

let make_list_of_victory_points players board game =
  let player_list = Array.to_list players in
  build_list player_list board game

let max_victory_points players board game =
  list_max (make_list_of_victory_points players board game)
