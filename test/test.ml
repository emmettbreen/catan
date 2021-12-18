open OUnit2
open Catan
open Player
open Game
open Board

let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let player_test_get_num_resource
    (name : string)
    (expected_output : int)
    (pl : Player.t)
    (res : resource) =
  name >:: fun _ ->
  assert_equal expected_output (get_num_resource pl res)

let player_test_add_resources
    (name : string)
    (expected_output : Player.t)
    (pl : Player.t)
    (res : (resource * int) list) =
  name >:: fun _ -> assert_equal expected_output (add_resources pl res)

let player_test_get_num_victory_points
    (name : string)
    (expected_output : int)
    (pl : Player.t) =
  name >:: fun _ ->
  assert_equal expected_output (get_num_victory_points pl)

let player_test_dec_num_victory_points
    (name : string)
    (expected_output : Player.t)
    (pl : Player.t)
    (num : int) =
  name >:: fun _ ->
  assert_equal expected_output (dec_num_victory_ponts pl num)

let player_test_inc_num_victory_points
    (name : string)
    (expected_output : Player.t)
    (pl : Player.t)
    (num : int) =
  name >:: fun _ ->
  assert_equal expected_output (inc_num_victory_ponts pl num)

let player_test_get_player_id
    (name : string)
    (expected_output : player_id)
    (pl : Player.t) =
  name >:: fun _ -> assert_equal expected_output (get_player_id pl)

let player_test_trade_resource_success
    (name : string)
    (expected_output : Player.t)
    (pl : Player.t)
    (resource1 : resource)
    (resource2 : resource) =
  name >:: fun _ ->
  assert_equal expected_output (trade_resource resource1 resource2 pl)

let player_test_trade_resource_failure
    (name : string)
    (expected_error_resource : resource)
    (pl : Player.t)
    (resource1 : resource)
    (resource2 : resource) =
  name >:: fun _ ->
  assert_raises (InsufficientResources expected_error_resource)
    (fun _ -> trade_resource resource1 resource2 pl)

let game_test_vp_from_cities_and_settlements
    (name : string)
    (expected_output : int)
    (pl : Player.t)
    (b : Board.t) =
  name >:: fun _ ->
  assert_equal expected_output (vp_from_cities_and_settlements pl b)

let game_test_update_board
    (name : string)
    (expected_output : Game.t)
    (game : Game.t)
    (board : Board.t) =
  name >:: fun _ ->
  assert_equal expected_output (update_board game board)

let game_test_update_players
    (name : string)
    (expected_output : Game.t)
    (game : Game.t)
    (players : Player.t array) =
  name >:: fun _ ->
  assert_equal expected_output (update_players game players)

let player = create_player "123456" [ (Wool, 2) ] 1 ANSITerminal.red

let player_with_more_resources =
  create_player "123456"
    [ (Brick, 5); (Ore, 2); (Wool, 5); (Grain, 1); (Lumber, 4) ]
    1 ANSITerminal.red

let player_with_more_victory_points =
  create_player "123456" [ (Wool, 2) ] 4 ANSITerminal.red

let player_after_successful_trade =
  create_player "123456"
    [ (Brick, 5); (Ore, 3); (Wool, 1); (Grain, 1); (Lumber, 4) ]
    1 ANSITerminal.red

let player_tests =
  [
    player_test_get_num_resource "Test get player resource for Brick" 0
      player Brick;
    player_test_get_num_resource "Test get player resource for Wool" 2
      player Wool;
    player_test_add_resources
      "Test add no resources to a player's resources" player player [];
    player_test_add_resources
      "Test add 5 Brick, 2 Ore, 3 Wool, 4 Lumber, and 1 Grain to a \
       player's resources"
      player_with_more_resources player
      [ (Brick, 5); (Ore, 2); (Wool, 3); (Grain, 1); (Lumber, 4) ];
    player_test_get_num_victory_points
      "Test getting victory points from a player" 1 player;
    player_test_inc_num_victory_points
      "Test increment victory points by 3 for a player"
      player_with_more_victory_points player 3;
    player_test_dec_num_victory_points
      "Test decrement victory points by 3 for a player" player
      player_with_more_victory_points 3;
    player_test_get_player_id "Test get a player's id" "123456" player;
    player_test_trade_resource_failure
      "Test failing to trade Wool for Ore for a player" Wool player Wool
      Ore;
    player_test_trade_resource_success
      "Test trade Wool for Ore for a player"
      player_after_successful_trade player_with_more_resources Wool Ore;
  ]

let board =
  gen_board
    [
      (Hill, 2);
      (Forest, 3);
      (Mountain, 3);
      (Field, 4);
      (Hill, 4);
      (Hill, 5);
      (Hill, 5);
      (Mountain, 6);
      (Forest, 6);
      (Desert, 7);
      (Hill, 8);
      (Hill, 8);
      (Forest, 9);
      (Field, 9);
      (Hill, 10);
      (Mountain, 10);
      (Field, 11);
      (Hill, 11);
      (Forest, 12);
    ]

let player1 =
  create_player "Player 1"
    [ (Brick, 1); (Lumber, 1); (Wool, 1); (Grain, 1) ]
    0 ANSITerminal.red

let board' = buy_sett_init board (get_player_id player1) 5

let board_w_p_sett = buy_sett_init board (get_player_id player) 0

let board_w_p_2sett = buy_sett_init board (get_player_id player) 18

let board_w_p_city = upgr_city 18 (get_player_id player) board_w_p_2sett

let board_w_p_city_road =
  buy_road_init board_w_p_2sett (get_player_id player) (18, 19)

(* test update function*)
(*let updates = get_updates 4 board_w_p_city_road*)

let test_board_with_owner_of_node
    (name : string)
    (expected_output : player_id)
    (board : t)
    (node_pos : node_position) =
  name >:: fun _ ->
  assert_equal expected_output (owner_of_node board node_pos)

let test_board_with_owner_of_edge
    (name : string)
    (expected_output : player_id)
    (board : t)
    (edge_pos : node_position * node_position) =
  name >:: fun _ ->
  assert_equal expected_output
    (owner_of_edge board (fst edge_pos) (snd edge_pos))

let pp_mylist (id, res) = "(" ^ id ^ ", " ^ " ? " ^ ")"

let test_updates_to_board
    (name : string)
    (expected_output : (Player.player_id * Player.resource) list)
    (roll : roll_number)
    (board : t)
    ~printer:pp_list
    pp_mylist =
  name >:: fun _ ->
  assert_equal expected_output
    (get_updates roll board)
    ~printer:(pp_list pp_mylist)

let test_ports
    (name : string)
    (expected_output : port list)
    (board : t)
    (player : player_id) =
  name >:: fun _ ->
  assert_equal expected_output (get_ports board player)

let board_tests =
  [
    test_board_with_owner_of_node "player has settlement on node 0"
      (get_player_id player) board_w_p_sett 0;
    test_board_with_owner_of_node "player has settlement on node 18"
      (get_player_id player) board_w_p_2sett 18;
    test_board_with_owner_of_node "player has city on node 18"
      (get_player_id player) board_w_p_city 18;
    ( "player trying to rebuy settlement at 0 should fail" >:: fun _ ->
      assert_raises InvalidPlacement (fun _ ->
          buy_sett 0 (get_player_id player) board_w_p_2sett) );
    ( "player trying to buy city at 14 should fail" >:: fun _ ->
      assert_raises InvalidPlacement (fun _ ->
          upgr_city 14 (get_player_id player) board_w_p_2sett) );
    test_board_with_owner_of_edge
      "player has road attatched to city on node 18"
      (get_player_id player) board_w_p_city_road (18, 19);
    ( "player trying to buy road between 20 and 21 should fail"
    >:: fun _ ->
      assert_raises InvalidPlacement (fun _ ->
          buy_road (20, 21) (get_player_id player) board_w_p_city_road)
    );
    test_updates_to_board "no one owns anything should return empy list"
      [] 6 board ~printer:pp_list pp_mylist;
    test_updates_to_board
      "player has settlement at node 0, is on hex 0, \n\
      \    should get one resource of type Brick when roll number of \
       hex 0 is rolled"
      [ (get_player_id player, Brick) ]
      2 board_w_p_sett ~printer:pp_list pp_mylist;
    test_updates_to_board
      "player has city at node 18, is on hex 3, \n\
      \    should get two resources of type Grain when roll number of \
       hex 3 is rolled"
      [ (get_player_id player, Grain); (get_player_id player, Grain) ]
      4 board_w_p_city ~printer:pp_list pp_mylist;
    test_ports "player has\n      port ThreeToOne since they own node 0"
      [ ThreeToOne ] board_w_p_sett (get_player_id player);
  ]

let players =
  [|
    player; player_with_more_resources; player_with_more_victory_points;
  |]

let new_players =
  [|
    player;
    player_with_more_resources;
    player_with_more_victory_points;
    player_after_successful_trade;
  |]

let game = init_game players board

let game_with_new_board = init_game players board_w_p_2sett

let game_with_new_players = init_game new_players board

let game_tests =
  [
    game_test_update_board "Test update game with a new board"
      game_with_new_board game board_w_p_2sett;
    game_test_update_board "Test update game with the same board" game
      game board;
    game_test_update_players
      "Test update game with the same list of players" game game players;
    (* game_test_update_players "Test update game with the different
       list of players" game_with_new_players game new_players; *)
    game_test_vp_from_cities_and_settlements
      "Test city/settlement vps of board with no settlement and no \
       cities"
      0 player1 board;
    (*game_test_vp_from_cities_and_settlements "Test city/settlement vps
      of board with 1 settlement and no \ cities" 1 player1 board';*)
  ]

let suite =
  "test suite for Catan"
  >::: List.flatten [ player_tests; board_tests; game_tests ]

let _ = run_test_tt_main suite
