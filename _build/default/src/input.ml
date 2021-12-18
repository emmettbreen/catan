exception InvalidResource
(*helpers***********************************************************************)

let string_to_resource s =
  match s with
  | "Brick" -> Player.Brick
  | "Lumber" -> Player.Lumber
  | "Ore" -> Player.Ore
  | "Wool" -> Player.Wool
  | "Grain" -> Player.Grain
  | "brick" -> Player.Brick
  | "lumber" -> Player.Lumber
  | "ore" -> Player.Ore
  | "wool" -> Player.Wool
  | "grain" -> Player.Grain
  | "None" -> Player.None
  | "none" -> Player.None
  | _ -> raise InvalidResource

(*converts user input of desired hexagon to hexagon number in the
  backend*)
let i_o_pos pos : int =
  let arr =
    [|
      -1;
      7;
      3;
      12;
      0;
      8;
      16;
      4;
      13;
      1;
      9;
      17;
      5;
      14;
      2;
      10;
      18;
      6;
      15;
      11;
    |]
  in
  arr.(pos)

let rec choose low high : int =
  try
    let choice = read_int () in
    if choice < low || choice > high then (
      print_endline
        ("Please enter a number in the range [" ^ string_of_int low
       ^ ", " ^ string_of_int high ^ "]");
      choose low high)
    else choice
  with
  | _ ->
      print_endline
        ("Please enter a number in the range [" ^ string_of_int low
       ^ ", " ^ string_of_int high ^ "]");
      choose low high

(*robber***********************************************************************)

(* user input of robber placement and moves robber in the backend*)
let rec move_robber board =
  print_endline
    "You have rolled a 7! Choose a new location for the robber.";
  Printers.yellow_print
    "Note: the board now tmeporarily shows locations.\n";
  print_endline
    "The new location must be an integer in the range [0, 18].\n\
    \    Enter input: ";
  try Board.rob (i_o_pos (choose 1 19)) board with
  | Board.RobberNotMoved -> move_robber board

(*updates**********************************************************************)
let apply_updates pair player =
  if fst pair = Player.get_player_id player then
    Player.player_with_new_resource player (snd pair)
  else player

let rec update_players_with_resources lst game =
  match lst with
  | [] -> ()
  | h :: t ->
      let apply_updates_with_h player = apply_updates h player in
      Game.set_players game
        (Array.map apply_updates_with_h (Game.get_players game));
      update_players_with_resources t game

(*get_settlement***************************************************************)
let rec build_settlement_aux player board game =
  try
    Board.buy_sett (choose 0 53)
      (Player.get_player_id (Game.get_current_player game))
      board
  with
  | Board.InvalidPlacement ->
      Printers.red_print
        "\nA settlement already exists here. Cannot build another. \n";
      build_settlement_aux player board game

let build_settlement player board game =
  print_endline
    "Which position do you want to build your settlement at?\n\
     Enter a value in the range [0, 53]: ";

  Game.set_curr_player game
    (Player.buy_settlement (Game.get_current_player game));
  build_settlement_aux player board game

(*get_city*********************************************************************)
let rec upgrade_settlement_aux player board game =
  try
    Board.upgr_city (choose 0 53)
      (Player.get_player_id (Game.get_current_player game))
      board
  with
  | Board.InvalidPlacement ->
      Printers.red_print
        "\nYou do not own a settlement here yet. Cannot build a city\n";
      upgrade_settlement_aux player board game

let upgrade_settlement player board game =
  print_endline
    "Which position do you want to build your city at?\n\
     Enter a value in the range [0, 53]: ";
  Game.set_curr_player game
    (Player.buy_city (Game.get_current_player game));
  upgrade_settlement_aux player board game

(*get_road*********************************************************************)

let rec build_road_aux player board game =
  try
    Board.buy_road
      (choose 0 53, choose 0 53)
      (Player.get_player_id (Game.get_current_player game))
      board
  with
  | Board.InvalidPlacement ->
      Printers.red_print "\nNot a valid road placement\n";
      build_road_aux player board game

let build_road player board game =
  print_endline
    "Which position do you want to build your road at?\n\
    \   Enter a 2 different space seperated values, each in the range \
     of [0, 53]: ";
  Game.set_curr_player game
    (Player.buy_road (Game.get_current_player game));
  build_road_aux player board game

(*trade************************************************************************)

let rec get_resources () =
  let () =
    print_string
      "Enter two space seperated resources of the format [owned \
       resource] [wanted resource]: "
  in
  let i =
    List.map
      (fun x -> string_to_resource x)
      (String.split_on_char ' ' (read_line ()))
  in
  match i with
  | [ x; y ] -> (x, y)
  | _ ->
      print_string
        "Try again. Input must be a valid space seperated pair of \
         resources of the form \n\
        \ [owned resource] [wanted resource]";
      get_resources ()

let bank_trade player board game =
  let owned_resource, wanted_resource = get_resources () in
  Game.set_curr_player game
    (Player.trade_resource owned_resource wanted_resource
       (Game.get_current_player game))

let player_trade player board game = failwith "unimplemented"

(*Player { trade_with; wishlist; givelist } -> UnitC (Game.trade
  trade_with wishlist givelist game)*)
(*let rec get_trade_num_input_player () = print_string "Who would you\n\
  \ like to trade with?\n\ \ Enter a valid id of another player: "; let
  i = read_line () in if check_trade_input_player i then i else (
  print_endline trade_num_input_player_prompt;
  get_trade_num_input_player ())

  let get_trade_num_input_wishlist () = List.map (fun x -> match x with
  | res, num -> (string_to_resource res, int_of_string num))
  (list_to_pair_list (String.split_on_char ' '
  (get_trade_num_input_player ())))

  let get_trade_num_input_givelist () = List.map (fun x -> match x with
  | res, num -> (string_to_resource res, int_of_string num))
  (list_to_pair_list (String.split_on_char ' '
  (get_trade_num_input_player ())))*)

let trade player board game =
  print_endline
    "Which kind of trade would you like to make?\n\
    \      1. Bank\n\
    \      2. Player\n\
    \      Enter a number: ";
  match choose 1 2 with
  | 1 -> bank_trade player board game
  | 2 -> player_trade player board game
  | _ -> failwith "guaranteed to never fail"

(*turns************************************************************************)

let rec turn player board game flag =
  (*phase 1 of turn, roll & resource allocation, robber if 7 rolled*)
  if flag then (
    Printers.cyan_print_endline
      ("" ^ (player |> Player.get_player_id |> String.trim) ^ "'s Turn:");
    let roll =
      Random.self_init ();
      Random.int 10 + 2
    in
    let new_resources = Board.get_updates roll board in
    update_players_with_resources new_resources game;
    Printers.green_print_endline ("Your roll: " ^ string_of_int roll);
    print_endline (Game.player_string player board game);
    if roll = 7 then
      (Board.print_board_alt board (Game.get_player_color_pairs game);
       turn player (move_robber board) game)
        false
    else turn player board game false)
  else Board.print_board board (Game.get_player_color_pairs game);
  (* phase 2 of turn, player decides what to do*)
  print_string
    "What would you like to do?\n\
    \ 1. Quit\n\
    \ 2. Trade resources\n\
    \ 3. Build a settlement\n\
    \ 4. Upgrade a settlement\n\
    \ 5. Build a road\n\
    \ 6. End Turn\n\n\
     Enter a number: ";
  (* matching input to outputs*)
  match choose 1 6 with
  | 1 ->
      Printers.red_print_endline "Thanks for playing!";
      exit 0
  | 2 ->
      trade player board game;
      turn player board game false
  | 3 -> turn player (build_settlement player board game) game false
  | 4 -> turn player (upgrade_settlement player board game) game false
  | 5 -> turn player (build_road player board game) game false
  | 6 ->
      Game.next_player game;
      turn (Game.get_current_player game) board game true
  | _ -> failwith "guaranteed to never fail here"
