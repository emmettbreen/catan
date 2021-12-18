open Printers

type port =
  | Brick
  | Lumber
  | Ore
  | Grain
  | Wool
  | ThreeToOne

type hexagon =
  | Hill
  | Forest
  | Mountain
  | Field
  | Pasture
  | Desert

type building =
  | City
  | Settlement

type node_position = int

type hex_position = int

type roll_number = int

exception InvalidNode

exception InvalidEdge

exception UnownedNode

exception UnknownNode

exception UnownedEdge

exception UnknownEdge

exception NoSettlement

exception InvalidPlacement

exception InvalidColor

exception RobberNotMoved

type hex = {
  hexagon_type : hexagon;
  position : hex_position;
  roll_number : roll_number;
  robbed : bool;
}

type edge = {
  position : node_position * node_position;
  owner : Player.player_id option;
}

type node = {
  position : node_position;
  hexagons : hex list;
  owner : Player.player_id option;
  building : building option;
  port : port option;
}

type t = {
  hexagons : hex list;
  nodes : node list;
  edges : edge list;
  robber_pos : int;
}

let rec get_node_from_board node_position = function
  | [] -> raise UnknownNode
  | h :: t ->
      if h.position = node_position then h
      else get_node_from_board node_position t

let rec get_edge_from_board
    (node_pos1 : node_position)
    (node_pos2 : node_position)
    (edges : edge list) =
  match edges with
  | [] -> raise UnknownEdge
  | { position = n1, n2; owner = pl } :: t ->
      if n1 = node_pos1 && n2 = node_pos2 then
        { position = (n1, n2); owner = pl }
      else get_edge_from_board node_pos1 node_pos2 t

let owner_of_node (board : t) (node_position : node_position) =
  let node = get_node_from_board node_position board.nodes in
  match node.owner with
  | Some pl -> pl
  | None -> raise UnownedNode

let owner_of_edge
    (board : t)
    (node_pos1 : node_position)
    (node_pos2 : node_position) =
  let edge = get_edge_from_board node_pos1 node_pos2 board.edges in
  match edge.owner with
  | Some pl -> pl
  | None -> raise UnownedEdge

let node_is_owned (board : t) (node_position : node_position) =
  let node = get_node_from_board node_position board.nodes in
  match node.owner with
  | Some pl -> true
  | None -> false

let edge_is_owned
    (board : t)
    (node_pos1 : node_position)
    (node_pos2 : node_position) =
  let edge = get_edge_from_board node_pos1 node_pos2 board.edges in
  match edge.owner with
  | Some pl -> true
  | None -> false

let rec get_player_edges_from_edge_list player (edges : edge list) =
  match edges with
  | [] -> []
  | h :: t ->
      if h.owner = Some (Player.get_player_id player) then
        h :: get_player_edges_from_edge_list player t
      else get_player_edges_from_edge_list player t

let get_player_edges_from_board player board =
  get_player_edges_from_edge_list player board.edges

let hex_to_res (hexagon : hexagon) : Player.resource =
  match hexagon with
  | Hill -> Brick
  | Mountain -> Ore
  | Forest -> Lumber
  | Pasture -> Wool
  | Field -> Grain
  | _ -> None

let get_resources (node : node) =
  List.map (fun x -> hex_to_res x.hexagon_type) node.hexagons

let get_ports (board : t) (player : Player.player_id) =
  let rec get_ports_aux nodes =
    match nodes with
    | [] -> []
    | h :: t ->
        if h.owner = Some player then
          match h.port with
          | None -> get_ports_aux t
          | Some p -> p :: get_ports_aux t
        else get_ports_aux t
  in
  get_ports_aux board.nodes

let get_node (board : t) (node_pos : node_position) : node =
  let rec search nodes =
    match nodes with
    | [] -> raise UnknownNode
    | h :: t when h.position = node_pos -> h
    | h :: t -> search t
  in
  search board.nodes

(* generate map helper*)
let hex_lst (input : (hexagon * roll_number) list) =
  let rec hex_lst_aux lst pos =
    match lst with
    | (hex, num) :: t ->
        {
          hexagon_type = hex;
          position = pos;
          roll_number = num;
          robbed = false;
        }
        :: hex_lst_aux t (pos + 1)
    | [] -> []
  in
  hex_lst_aux input 0

(* helper for hex_at. given a list of hexagons and a hex_position,
   returns the hexagon at the position*)
let rec hex_at_pos (hex_lst : hex list) (pos : hex_position) : hex =
  match hex_lst with
  | [] -> raise UnknownNode
  | hex :: t when hex.position = pos -> hex
  | hex :: t -> hex_at_pos t pos

(* helper for node_lst. calls hex_at_pos for each element in the list of
   hexagons (touching the node in node_lst)*)
let rec hex_at (hex_lst : hex list) (pos_list : hex_position list) :
    hex list =
  match pos_list with
  | num :: t -> hex_at_pos hex_lst num :: hex_at hex_lst t
  | [] -> []

(* length of [hex_at_positions] should be same as number of nodes.
   length of an list within positions is equal to the number of hexagon
   touching that node*)
let node_lst (all_hex : hex list) =
  let hex_at_positions =
    [
      [ 0 ];
      [ 0 ];
      [ 0; 1 ];
      [ 1 ];
      [ 1; 2 ];
      [ 2 ];
      [ 2 ];
      [ 3 ];
      [ 0; 3 ];
      [ 0; 3; 4 ];
      [ 0; 1; 4 ];
      [ 1; 4; 5 ];
      [ 1; 2; 5 ];
      [ 2; 5; 6 ];
      [ 2; 6 ];
      [ 6 ];
      [ 7 ];
      [ 3; 7 ];
      [ 3; 7; 8 ];
      [ 3; 4; 8 ];
      [ 4; 8; 9 ];
      [ 4; 5; 9 ];
      [ 5; 9; 10 ];
      [ 5; 6; 10 ];
      [ 6; 10; 11 ];
      [ 6; 11 ];
      [ 11 ];
      [ 7 ];
      [ 7; 12 ];
      [ 7; 8; 12 ];
      [ 8; 12; 13 ];
      [ 8; 9; 13 ];
      [ 9; 13; 14 ];
      [ 9; 10; 14 ];
      [ 10; 14; 15 ];
      [ 10; 11; 15 ];
      [ 11; 15 ];
      [ 11 ];
      [ 12 ];
      [ 12; 16 ];
      [ 12; 13; 16 ];
      [ 13; 16; 17 ];
      [ 13; 14; 17 ];
      [ 14; 17; 18 ];
      [ 14; 15; 18 ];
      [ 15; 18 ];
      [ 15 ];
      [ 16 ];
      [ 16 ];
      [ 16; 17 ];
      [ 17 ];
      [ 17; 18 ];
      [ 18 ];
      [ 18 ];
    ]
  in
  let rec node_lst_aux node_pos hex_positions =
    match hex_positions with
    | hexs_pos :: t ->
        {
          position = node_pos;
          hexagons = hex_at all_hex hexs_pos;
          owner = None;
          building = None;
          port = None;
        }
        :: node_lst_aux (node_pos + 1) t
    | [] -> []
  in
  node_lst_aux 0 hex_at_positions

let edge_positions =
  [
    (0, 1);
    (1, 2);
    (2, 3);
    (3, 4);
    (4, 5);
    (5, 6);
    (0, 8);
    (2, 10);
    (4, 12);
    (6, 14);
    (7, 8);
    (8, 9);
    (9, 10);
    (10, 11);
    (11, 12);
    (12, 13);
    (13, 14);
    (14, 15);
    (7, 17);
    (9, 19);
    (11, 21);
    (13, 23);
    (15, 25);
    (16, 17);
    (17, 18);
    (18, 19);
    (19, 20);
    (20, 21);
    (21, 22);
    (22, 23);
    (23, 24);
    (24, 25);
    (16, 27);
    (18, 29);
    (20, 31);
    (22, 33);
    (24, 35);
    (25, 26);
    (26, 37);
    (27, 28);
    (28, 29);
    (29, 30);
    (30, 31);
    (31, 32);
    (32, 33);
    (33, 34);
    (34, 35);
    (35, 36);
    (36, 37);
    (28, 38);
    (30, 40);
    (32, 42);
    (34, 44);
    (36, 46);
    (38, 39);
    (39, 40);
    (40, 41);
    (41, 42);
    (42, 43);
    (43, 44);
    (44, 45);
    (45, 46);
    (39, 47);
    (41, 49);
    (43, 51);
    (45, 53);
    (47, 48);
    (48, 49);
    (49, 50);
    (50, 51);
    (51, 52);
    (52, 53);
  ]

let order (a, b) = if a > b then (b, a) else (a, b)

let is_valid_edge e =
  let ordered = order e in
  List.filter (fun (a, b) -> (a, b) = ordered) edge_positions <> []

let edge_lst (nodes : node list) =
  let rec edge_lst_aux edge_pos =
    match edge_pos with
    | (a, b) :: t ->
        { position = (a, b); owner = None } :: edge_lst_aux t
    | [] -> []
  in
  edge_lst_aux edge_positions

let rec add_ports_to_board (nodes : node list) : node list =
  match nodes with
  | [] -> []
  | node :: t when node.position = 0 || node.position = 1 ->
      { node with port = Some ThreeToOne } :: add_ports_to_board t
  | node :: t when node.position = 3 || node.position = 4 ->
      { node with port = Some Lumber } :: add_ports_to_board t
  | node :: t when node.position = 14 || node.position = 15 ->
      { node with port = Some Brick } :: add_ports_to_board t
  | node :: t when node.position = 7 || node.position = 17 ->
      { node with port = Some Grain } :: add_ports_to_board t
  | node :: t when node.position = 26 || node.position = 37 ->
      { node with port = Some ThreeToOne } :: add_ports_to_board t
  | node :: t when node.position = 28 || node.position = 38 ->
      { node with port = Some Ore } :: add_ports_to_board t
  | node :: t when node.position = 45 || node.position = 46 ->
      { node with port = Some ThreeToOne } :: add_ports_to_board t
  | node :: t when node.position = 47 || node.position = 48 ->
      { node with port = Some ThreeToOne } :: add_ports_to_board t
  | node :: t when node.position = 50 || node.position = 51 ->
      { node with port = Some Wool } :: add_ports_to_board t
  | node :: t -> node :: add_ports_to_board t

let rob (hex_pos : hex_position) (board : t) : t =
  if board.robber_pos = hex_pos then raise RobberNotMoved
  else
    let updated_hexs =
      List.map
        (fun (h : hex) ->
          if h.position = hex_pos then { h with robbed = true }
          else if h.position = board.robber_pos then
            { h with robbed = false }
          else h)
        board.hexagons
    in
    { board with hexagons = updated_hexs; robber_pos = hex_pos }

let gen_board (input : (hexagon * roll_number) list) : t =
  rob 9
    {
      hexagons = hex_lst input;
      nodes = input |> hex_lst |> node_lst |> add_ports_to_board;
      edges = input |> hex_lst |> node_lst |> edge_lst;
      robber_pos = -1;
    }

let get_robber_pos (board : t) : hex_position = board.robber_pos

let get_adj_nodes (board : t) (node : node_position) :
    node_position list =
  let edges =
    List.filter
      (fun (e : edge) -> fst e.position = node || snd e.position = node)
      board.edges
  in
  let rec get_from_edges (lst : edge list) : node_position list =
    match lst with
    | [] -> []
    | e :: t ->
        if fst e.position = node then snd e.position :: get_from_edges t
        else if snd e.position = node then
          fst e.position :: get_from_edges t
        else get_from_edges t
  in
  get_from_edges edges

let own_an_adj_edge
    (board : t)
    (node : node_position)
    (pl : Player.player_id) : bool =
  let adj_edges =
    List.filter
      (fun (e : edge) -> fst e.position = node || snd e.position = node)
      board.edges
  in
  let rec check (lst : edge list) =
    match lst with
    | [] -> false
    | edge :: t -> if edge.owner = Some pl then true else check t
  in
  check adj_edges

let rec all_adj_nodes_open (board : t) = function
  | [] -> true
  | n :: t ->
      (not (node_is_owned board n)) && all_adj_nodes_open board t

let buy_sett_func
    (board : t)
    (player : Player.player_id)
    (node_pos : node_position) =
  {
    hexagons = board.hexagons;
    nodes =
      List.map
        (fun x ->
          if x.position = node_pos && x.owner = None then
            { x with owner = Some player; building = Some Settlement }
          else x)
        board.nodes;
    edges = board.edges;
    robber_pos = board.robber_pos;
  }

let buy_sett
    (node_pos : node_position)
    (player : Player.player_id)
    (board : t) =
  if
    all_adj_nodes_open board (node_pos :: get_adj_nodes board node_pos)
    && own_an_adj_edge board node_pos player
  then buy_sett_func board player node_pos
  else raise InvalidPlacement

let buy_sett_init
    (board : t)
    (player : Player.player_id)
    (node_pos : node_position) =
  if
    (not (node_is_owned board node_pos))
    && all_adj_nodes_open board (get_adj_nodes board node_pos)
  then buy_sett_func board player node_pos
  else raise InvalidPlacement

let upgr_city
    (node_pos : node_position)
    (player : Player.player_id)
    (board : t) =
  if
    node_is_owned board node_pos
    && owner_of_node board node_pos = player
    && (get_node board node_pos).building = Some Settlement
  then
    {
      hexagons = board.hexagons;
      nodes =
        List.map
          (fun x ->
            if
              x.position = node_pos && x.owner = Some player
              && x.building = Some Settlement
            then { x with building = Some City }
            else x)
          board.nodes;
      edges = board.edges;
      robber_pos = board.robber_pos;
    }
  else raise InvalidPlacement

let buy_road_func
    (board : t)
    (player : Player.player_id)
    (edge_pos : node_position * node_position) : t =
  {
    board with
    edges =
      List.map
        (fun (x : edge) ->
          if x.position = edge_pos then
            { position = x.position; owner = Some player }
          else x)
        board.edges;
  }

let buy_road
    (edge_pos : node_position * node_position)
    (player : Player.player_id)
    (board : t) : t =
  let edge = order edge_pos in

  if List.filter (fun (a, b) -> (a, b) = edge) edge_positions = [] then
    raise InvalidEdge
  else if
    (own_an_adj_edge board (fst edge) player
    || own_an_adj_edge board (snd edge) player)
    && not (edge_is_owned board (fst edge) (snd edge))
  then buy_road_func board player edge
  else raise InvalidPlacement

let buy_road_init
    (board : t)
    (player : Player.player_id)
    (edge_pos : node_position * node_position) : t =
  let edge = order edge_pos in
  if List.filter (fun (a, b) -> (a, b) = edge) edge_positions = [] then
    raise InvalidEdge
  else if
    node_is_owned board (fst edge)
    && owner_of_node board (fst edge) = player
    || node_is_owned board (snd edge)
       && owner_of_node board (snd edge) = player
  then buy_road_func board player edge
  else raise InvalidPlacement

let rec nodes_on_hex (nodes : node list) (hex : hex) :
    (node * hexagon) list =
  match nodes with
  | [] -> []
  | node :: other_nodes -> (
      match node.hexagons with
      | [] -> nodes_on_hex other_nodes hex
      | h :: t when h = hex ->
          if node.building = Some Settlement then
            (node, h.hexagon_type) :: nodes_on_hex other_nodes hex
          else
            (node, h.hexagon_type) :: (node, h.hexagon_type)
            :: nodes_on_hex other_nodes hex
      | h :: t -> nodes_on_hex other_nodes hex)

let rec nodes_on_hexs (nodes : node list) (hexs : hex list) :
    (node * hexagon) list =
  match hexs with
  | [] -> []
  | h :: t -> nodes_on_hex nodes h @ nodes_on_hexs nodes t

let get_updates (roll_num : roll_number) (board : t) :
    (Player.player_id * Player.resource) list =
  let affected_hexs =
    List.filter
      (fun hex -> hex.roll_number = roll_num && not hex.robbed)
      board.hexagons
  in
  let node_hex_lst =
    nodes_on_hexs
      (List.filter (fun n -> n.owner <> None) board.nodes)
      affected_hexs
  in
  let rec get_owner (lst : (node * hexagon) list) :
      (Player.player_id * Player.resource) list =
    match lst with
    | [] -> []
    | (n, h) :: t -> (
        match n.owner with
        | Some o -> (o, hex_to_res h) :: get_owner t
        | None -> get_owner t)
  in
  get_owner node_hex_lst

let get_node_settlement (board : t) (node_position : node_position) =
  let node = get_node_from_board node_position board.nodes in
  match node.building with
  | Some building -> building
  | None -> raise NoSettlement

let get_nodes board = board.nodes

let get_style_from_building node =
  match node.building with
  | None -> []
  | Some building -> (
      match building with
      | City -> [ ANSITerminal.Background ANSITerminal.Black ]
      | Settlement -> [ ANSITerminal.Background ANSITerminal.White ])

let print_port board node_position empty_side =
  let node = get_node board node_position in
  match node.port with
  | None -> print_string ""
  | Some port -> (
      match port with
      | Brick -> print_string "🧱"
      | Lumber ->
          let output = if empty_side = "left" then " 🪵" else "🪵 " in
          print_string output
      | Ore -> print_string "🪨"
      | Grain -> print_string "🌾"
      | Wool -> print_string "🐑"
      | ThreeToOne ->
          let output = if empty_side = "left" then " 3️⃣" else "3️⃣ " in
          print_string output)

let rec get_color_from_player_id colors player_id =
  match colors with
  | [] -> raise InvalidColor
  | (id, color) :: t ->
      if id = player_id then color
      else get_color_from_player_id t player_id

let print_node colors board node_position =
  let string_of_node_pos =
    if node_position < 10 then "0" ^ string_of_int node_position
    else string_of_int node_position
  in
  if node_is_owned board node_position then
    style_printing
      ((owner_of_node board node_position
       |> get_color_from_player_id colors)
      :: get_style_from_building
           (get_node_from_board node_position board.nodes))
      string_of_node_pos
  else white_print string_of_node_pos

let print_edge colors board node_pos_1 node_pos_2 edge_str =
  if edge_is_owned board node_pos_1 node_pos_2 then
    color_print
      (owner_of_edge board node_pos_1 node_pos_2
      |> get_color_from_player_id colors)
      edge_str
  else white_print edge_str

(* Helper for print_hex_tiles *)
let rec loop_elements (acc : string) (str : string) (times : int) =
  if times = 0 then acc else str ^ loop_elements acc str (times - 1)

let print_hex_tiles board hex_pos =
  let hex = hex_at_pos board.hexagons hex_pos in
  match hex.hexagon_type with
  | Hill -> print_string (loop_elements "" "⛰️" 6)
  | Forest -> print_string (loop_elements "" "🌲" 3)
  | Mountain -> print_string (loop_elements "" "🏔️" 6)
  | Field -> print_string (loop_elements "" "🌾" 3)
  | Pasture -> print_string (loop_elements "" "🌿" 3)
  | Desert -> print_string (loop_elements "" "🏜️" 6)

let print_hex_roll_num board hex_pos =
  magenta_print
    (let hex = hex_at_pos board.hexagons hex_pos in
     let num =
       if hex.roll_number < 10 then "0" ^ string_of_int hex.roll_number
       else string_of_int hex.roll_number
     in
     if hex.robbed then " 🔫" ^ num ^ "🔫 " else "   " ^ num ^ "   ")

let print_hex_num =
  let num = ref 0 in
  fun board hex_pos ->
    let hex = hex_at_pos board.hexagons hex_pos in
    num := !num + 1;
    if hex.robbed then magenta_print (" 🔫" ^ string_of_int !num ^ "🔫 ")
    else magenta_print ("   " ^ string_of_int !num ^ "   ")

let board_printer board colors (f : t -> int -> unit) =
  (* 1st line *)
  white_print {|              |};
  print_port board 7 "right";
  print_string {|   |};
  print_node colors board 16;
  print_edge colors board 16 27 "--";
  print_node colors board 27;
  print_string {|   |};
  print_port board 28 "left";
  white_print_endline "";
  (* 2nd line *)
  white_print {|             |};
  magenta_print "/";
  magenta_print "  \\ ";
  print_edge colors board 16 17 "/";
  print_hex_tiles board 7;
  print_edge colors board 27 28 "\\";
  magenta_print " /";
  magenta_print "  \\";
  white_print_endline "";
  (* 3rd line *)
  white_print {|            |};
  print_node colors board 7;
  print_edge colors board 07 17 "--";
  print_node colors board 17;
  f board 7;
  print_node colors board 28;
  print_edge colors board 28 38 "--";
  print_node colors board 38;
  white_print_endline "";
  (* 4th line *)
  white_print {|           |};
  print_edge colors board 7 8 "/";
  print_hex_tiles board 3;
  print_edge colors board 17 18 "\\";
  print_hex_tiles board 7;
  print_edge colors board 28 29 "/";
  print_hex_tiles board 12;
  print_edge colors board 38 39 "\\";
  white_print_endline "";
  (* 5th line *)
  white_print {|  |};
  print_port board 0 "right";
  magenta_print "-";
  print_node colors board 0;
  print_edge colors board 0 8 "--";
  print_node colors board 8;
  f board 3;
  print_node colors board 18;
  print_edge colors board 18 29 "--";
  print_node colors board 29;
  f board 12;
  print_node colors board 39;
  print_edge colors board 39 47 "--";
  print_node colors board 47;
  magenta_print "-";
  print_port board 47 "left";
  white_print_endline "";
  (* 6th line *)
  white_print {|  |};
  magenta_print "| ";
  print_edge colors board 0 1 "/";
  print_hex_tiles board 0;
  print_edge colors board 8 9 "\\";
  print_hex_tiles board 3;
  print_edge colors board 18 19 "/";
  print_hex_tiles board 8;
  print_edge colors board 29 30 "\\";
  print_hex_tiles board 12;
  print_edge colors board 39 40 "/";
  print_hex_tiles board 16;
  print_edge colors board 47 48 "\\";
  magenta_print " |";
  white_print_endline "";
  (* 7th line *)
  white_print {|  |};
  print_node colors board 1;
  f board 0;
  print_node colors board 9;
  print_edge colors board 9 19 "--";
  print_node colors board 19;
  f board 8;
  print_node colors board 30;
  print_edge colors board 30 40 "--";
  print_node colors board 40;
  f board 16;
  print_node colors board 48;
  white_print_endline "";
  (* 8th line *)
  white_print {|    |};
  print_edge colors board 1 2 "\\";
  print_hex_tiles board 0;
  print_edge colors board 9 10 "/";
  print_hex_tiles board 4;
  print_edge colors board 19 20 "\\";
  print_hex_tiles board 8;
  print_edge colors board 30 31 "/";
  print_hex_tiles board 13;
  print_edge colors board 40 41 "\\";
  print_hex_tiles board 16;
  print_edge colors board 48 49 "/";
  white_print_endline "";
  (* 9th line *)
  white_print {|     |};
  print_node colors board 2;
  print_edge colors board 2 10 "--";
  print_node colors board 10;
  f board 4;
  print_node colors board 20;
  print_edge colors board 20 31 "--";
  print_node colors board 31;
  f board 13;
  print_node colors board 41;
  print_edge colors board 41 49 "--";
  print_node colors board 49;
  white_print_endline "";
  (* 10th line *)
  white_print {|    |};
  print_edge colors board 2 3 "/";
  print_hex_tiles board 1;
  print_edge colors board 10 11 "\\";
  print_hex_tiles board 4;
  print_edge colors board 20 21 "/";
  print_hex_tiles board 9;
  print_edge colors board 31 32 "\\";
  print_hex_tiles board 13;
  print_edge colors board 41 42 "/";
  print_hex_tiles board 17;
  print_edge colors board 49 50 "\\";
  white_print_endline "";
  (* 11th line *)
  white_print {|  |};
  print_node colors board 3;
  f board 1;
  print_node colors board 11;
  print_edge colors board 11 21 "--";
  print_node colors board 21;
  f board 9;
  print_node colors board 32;
  print_edge colors board 32 42 "--";
  print_node colors board 42;
  f board 17;
  print_node colors board 50;
  white_print_endline "";
  (* 12th line *)
  white_print {|  |};
  magenta_print "| ";
  print_edge colors board 3 4 "\\";
  print_hex_tiles board 1;
  print_edge colors board 11 12 "/";
  print_hex_tiles board 5;
  print_edge colors board 21 22 "\\";
  print_hex_tiles board 9;
  print_edge colors board 32 33 "/";
  print_hex_tiles board 14;
  print_edge colors board 42 43 "\\";
  print_hex_tiles board 17;
  print_edge colors board 50 51 "/";
  magenta_print " |";
  white_print_endline "";
  (* 13th line *)
  white_print {|  |};
  print_port board 3 "right";
  magenta_print "-";
  print_node colors board 4;
  print_edge colors board 4 12 "--";
  print_node colors board 12;
  f board 5;
  print_node colors board 22;
  print_edge colors board 22 33 "--";
  print_node colors board 33;
  f board 14;
  print_node colors board 43;
  print_edge colors board 43 51 "--";
  print_node colors board 51;
  magenta_print "-";
  print_port board 3 "left";
  white_print_endline "";
  (* 14th line *)
  white_print {|    |};
  print_edge colors board 4 5 "/";
  print_hex_tiles board 2;
  print_edge colors board 12 13 "\\";
  print_hex_tiles board 5;
  print_edge colors board 22 23 "/";
  print_hex_tiles board 10;
  print_edge colors board 33 34 "\\";
  print_hex_tiles board 14;
  print_edge colors board 43 44 "/";
  print_hex_tiles board 18;
  print_edge colors board 51 52 "\\";
  white_print_endline "";
  (* 15th line *)
  white_print {|  |};
  print_node colors board 5;
  f board 2;
  print_node colors board 13;
  print_edge colors board 13 23 "--";
  print_node colors board 23;
  f board 10;
  print_node colors board 34;
  print_edge colors board 34 44 "--";
  print_node colors board 44;
  f board 18;
  print_node colors board 52;
  white_print_endline "";
  (* 16th line *)
  white_print {|    |};
  print_edge colors board 5 6 "\\";
  print_hex_tiles board 2;
  print_edge colors board 13 14 "/";
  print_hex_tiles board 6;
  print_edge colors board 23 24 "\\";
  print_hex_tiles board 10;
  print_edge colors board 34 35 "/";
  print_hex_tiles board 15;
  print_edge colors board 44 45 "\\";
  print_hex_tiles board 18;
  print_edge colors board 52 53 "/";
  white_print_endline "";
  (* 17th line *)
  white_print {|     |};
  print_node colors board 6;
  print_edge colors board 6 14 "--";
  print_node colors board 14;
  f board 6;
  print_node colors board 24;
  print_edge colors board 24 35 "--";
  print_node colors board 35;
  f board 15;
  print_node colors board 45;
  print_edge colors board 45 53 "--";
  print_node colors board 53;
  white_print_endline "";
  (* 18th line *)
  white_print {|         |};
  magenta_print "| ";
  print_edge colors board 14 15 "\\";
  print_hex_tiles board 6;
  print_edge colors board 24 25 "/";
  print_hex_tiles board 11;
  print_edge colors board 35 36 "\\";
  print_hex_tiles board 15;
  print_edge colors board 45 46 "/";
  magenta_print " |";
  white_print_endline "";
  (* 19th line *)
  white_print {|         |};
  print_port board 15 "right";
  magenta_print "-";
  print_node colors board 15;
  print_edge colors board 15 25 "--";
  print_node colors board 25;
  f board 11;
  print_node colors board 36;
  print_edge colors board 36 46 "--";
  print_node colors board 46;
  magenta_print "-";
  print_port board 45 "left";
  white_print_endline "";
  (* 20th line *)
  white_print {|                  |};
  print_edge colors board 25 26 "\\";
  print_hex_tiles board 11;
  print_edge colors board 36 37 "/";
  white_print_endline "";
  (* 21th line *)
  white_print {|                   |};
  print_node colors board 26;
  print_edge colors board 26 37 "--";
  print_node colors board 37;
  white_print_endline "";
  (*22th line*)
  white_print {|                    |};
  magenta_print "\\";
  magenta_print "  /";
  white_print_endline "";
  (* last line *)
  white_print {|                     |};
  print_port board 26 "right";
  white_print_endline ""

let print_board board colors =
  board_printer board colors print_hex_roll_num

let print_board_alt board colors =
  board_printer board colors print_hex_num
