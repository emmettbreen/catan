open Catan
open Player
open Game
open Board
open Printers
open Input

(* generates a random number < 100, then > 100, then < 100... *)
let oscillate_rand =
  let mem : int ref = ref 0 in
  fun () : int ->
    Random.self_init ();
    if !mem > 100 then (
      mem := Random.int 100;
      !mem)
    else (
      mem := Random.int 100 + 101;
      !mem)

(* shuffles [lst] with [input] maintainin the middle *)
let randomize_board_list lst input =
  lst
  |> List.map (fun x ->
         if x = input then (100, input) else (oscillate_rand (), x))
  |> List.sort (fun (x1, t1) (x2, t2) ->
         if x2 > x1 then 1 else if x2 < x1 then -1 else 0)
  |> List.map (fun (x, t) -> t)

(* genrates a random board [b] to be input to [gen_board] *)
let gen_rand_board_layout () =
  let types =
    randomize_board_list
      [
        Forest;
        Forest;
        Forest;
        Forest;
        Hill;
        Hill;
        Hill;
        Hill;
        Field;
        Desert;
        Field;
        Field;
        Field;
        Mountain;
        Mountain;
        Mountain;
        Pasture;
        Pasture;
        Pasture;
      ]
      Desert
  in
  let nums =
    randomize_board_list
      [ 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 8; 8; 9; 9; 10; 10; 11; 11; 12 ]
      7
  in
  List.fold_left2 (fun acc num typ -> (num, typ) :: acc) [] types nums

(* user inputs player info *)
let rec get_pl () : int =
  try
    let num_pl = read_int () in
    if num_pl < 2 || num_pl > 4 then (
      print_endline "There must be between 2 and 4 players";
      get_pl ())
    else num_pl
  with
  | Failure _ ->
      print_endline "You must enter a valid number between 2 and 4.";
      get_pl ()

(* user decides on board type *)
let rec get_board_num () : int =
  try
    let num_board = read_int () in
    if num_board < 0 || num_board > 2 then (
      print_endline "Please enter a 1 or a 2";
      get_board_num ())
    else num_board
  with
  | Failure _ ->
      print_endline "You must enter a valid number (1 or 2)";
      get_board_num ()

(* collect initial info *)
let init () =
  print_endline "Welcome to Catan";

  print_endline
    "Which board layout would you like to use?\n1) Default\n2) Random";

  let board_num = get_board_num () in
  let board =
    if board_num = 1 then
      gen_board
        [
          (Hill, 5);
          (Forest, 3);
          (Mountain, 3);
          (Field, 11);
          (Pasture, 8);
          (Hill, 5);
          (Forest, 2);
          (Mountain, 6);
          (Forest, 12);
          (Desert, 7);
          (Hill, 4);
          (Pasture, 8);
          (Forest, 9);
          (Field, 9);
          (Pasture, 10);
          (Mountain, 10);
          (Field, 4);
          (Hill, 11);
          (Forest, 6);
        ]
    else gen_board (gen_rand_board_layout ())
  in
  print_endline "How many players will be playing?";
  let num_pl = get_pl () in
  let players =
    Array.make num_pl (init_player "" ANSITerminal.yellow)
  in
  let rec collect_pl_info (counter : int) =
    print_endline ("Name of player " ^ string_of_int counter ^ " : ");
    let pl_name = read_line () in
    Array.set players (counter - 1)
      (init_player pl_name ANSITerminal.yellow);
    if counter = num_pl then () else collect_pl_info (counter + 1)
  in
  collect_pl_info 1;
  init_game players board

(* ensure settlement input is valid *)
let rec try_buy_sett (currstate : Game.t) (sett_pos : int) : Board.t =
  try
    buy_sett_init (get_board currstate)
      (get_player_id (get_current_player currstate))
      sett_pos
  with
  | InvalidPlacement ->
      red_print
        "Invalid Placement of Settlement, Please Try a Different \
         Location\n";
      try_buy_sett currstate (read_int ())
  | UnknownNode ->
      red_print
        "Invalid Placement of Settlement, Please Try a Different \
         Location\n";
      try_buy_sett currstate (read_int ())
  | Failure _ ->
      red_print "Invalid input. Please enter a valid location \n";
      try_buy_sett currstate (read_int ())

(* ensure road input is valid *)
let rec try_buy_road
    (board : Board.t)
    (currstate : Game.t)
    (road_pos : int * int) : Board.t =
  try
    buy_road_init board
      (get_player_id (get_current_player currstate))
      road_pos
  with
  | InvalidPlacement ->
      red_print
        "Invalid Placement of Road, Please Try a Different Location\n";
      try_buy_road board currstate (read_int (), read_int ())
  | InvalidEdge ->
      red_print "Invalid Road, Please Try a Different Location\n";
      try_buy_road board currstate (read_int (), read_int ())
  | Failure _ ->
      red_print "Invalid input. Please enter a valid location \n";
      try_buy_road board currstate (read_int (), read_int ())

(* first two turns for each player handler *)
let init_placements (state : Game.t) =
  print_endline
    "We will now take the next two turns to place your first \
     settlements and roads";
  let rec pl_placement (currstate : Game.t) : Game.t =
    print_board (get_board currstate)
      (Game.get_player_color_pairs state);
    print_endline
      ("Player "
      ^ string_of_int (get_curr_pl_ind currstate + 1)
      ^ " is up. Choose the position of your settlement");

    (* user input settlement position *)
    let sett_pos =
      try read_int () with
      | Failure _ ->
          red_print "Please enter a valid settlement postion.\n";
          read_int ()
    in
    let board_n_sett = try_buy_sett currstate sett_pos in

    print_board board_n_sett (Game.get_player_color_pairs state);

    print_endline
      ("Player "
      ^ string_of_int (get_curr_pl_ind currstate + 1)
      ^ ", choose the position of your road");

    (* user input road position*)
    let road_pos =
      try (read_int (), read_int ()) with
      | Failure _ ->
          red_print "Please enter a valid road position. \n";
          (read_int (), read_int ())
    in
    let board_n_sett_road =
      try_buy_road board_n_sett currstate road_pos
    in
    if
      get_curr_pl_ind currstate + 1
      = Array.length (get_players currstate)
    then update_board currstate board_n_sett_road
    else (
      next_player currstate;
      print_endline "";
      pl_placement (update_board currstate board_n_sett_road))
  in
  let round1 = pl_placement state in
  next_player round1;
  pl_placement round1

(* initialize board, collect info, handle first two turns *)
let started_game = init () |> init_placements

(* transition to normal game loop *)
let _ =
  print_board
    (get_board started_game)
    (Game.get_player_color_pairs started_game);
  print_endline "\nNow we will begin the actual game"

(* begin normal turns and gameplay *)
let t =
  turn
    (get_current_player started_game)
    (get_board started_game)
    started_game true
