(** Representation of a board in the game Catan.

    This module contains all of the functions and type definitions for
    boards, including resources, spaces (nodes), roads (edges), the
    robber, and more. It handles actions that would affect the board
    state as a whole, such as building property or moving the robber. *)

exception UnownedNode

exception UnknownNode

exception UnownedEdge

exception UnknownEdge

exception InvalidNode

exception InvalidEdge

exception InvalidPlacement

exception NoSettlement

exception InvalidColor

exception RobberNotMoved

type node_position = int

type hex_position = int

type roll_number = int

type port =
  | Brick
  | Lumber
  | Ore
  | Grain
  | Wool
  | ThreeToOne  (** The type representing ports in the Catan map*)

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
      (** The type representing possible building to be on the Catan map*)

type hex
(** The type corresponding to each hexagon that has been set up on the
    board*)

type node
(** The type corresponding to a place to put a settlement/city on the
    board Note for group: The owner field will have Some player_id if
    owned*)

type edge
(** The type corresponding to a place to put a road on the board. Note
    for group: The owner field will have Some player_id if owned*)

type t
(** The abstract type of values that represent a board in Catan. *)

val owner_of_node : t -> node_position -> Player.player_id
(** [owner c n] is a player_id * building option value of the owner of
    the node [n] on the board coupled with which type of building they
    own there. Requires: [c] is a valid catan map and [n] is a valid
    node*)

val owner_of_edge :
  t -> node_position -> node_position -> Player.player_id
(** [owner_of_edge c edg] is a player_id option value of the owner of
    road [edg]. Requires: [c] is a valid catan map and [edg] is a valid
    edge*)

val node_is_owned : t -> node_position -> bool
(** [node_is_owned gr n] is a boolean value that shows whether or not
    the location [n] in a graph [gr] is owned by a player*)

val edge_is_owned : t -> node_position -> node_position -> bool
(** [edge_is_owned gr pos1 pos2] is a boolean value that shows whether
    or not the edge between node positions [p1] and [p2] in a graph [gr]
    is owned by a player*)

val is_valid_edge : node_position * node_position -> bool

val get_ports : t -> Player.player_id -> port list
(** [get_ports c pl] is a list of ports that [pl] pwns on catan map [c]*)

val gen_board : (hexagon * roll_number) list -> t
(** [gen_board hex_roll_lst c] is a valid catan map [c] with given
    hexagons and associated rolls to collect resources from the given
    hexagon. Requires: [hex_roll_list] is length 18 since there are 18
    hexagons on the map*)

val get_nodes : t -> node list

val buy_sett : node_position -> Player.player_id -> t -> t
(** [buy_sett n pl c] is catan map [c] with [pl] the owner of node at
    position [n] on the map. Raises: [InvalidPlacement] if node at [n]
    or any adjacent node to [n] is already owned by anyone or [n] is not
    connected to a road that [pl] owns. Requires: [c] is a valid catan
    map and [n] is a valid node. [pl] must correspond to valid player.
    [pl] must have sufficient resources to make purchase. Resources of
    [pl] unchanged by [buy_sett c pl n]*)

val buy_sett_init : t -> Player.player_id -> node_position -> t
(** [buy_sett c pl n] is catan map [c] with [pl] the owner of node at
    position [n] on the map. Raises: [InvalidPlacement] if node at [n]
    or any adjacent node to [n] is already owned by anyone. Requires:
    [c] is a valid catan map and [n] is a valid node. [pl] must
    correspond to valid player.*)

val upgr_city : node_position -> Player.player_id -> t -> t
(** [upgr_city n pl c] is a catan map [c] with the settlement owned by
    [pl] at node [n] in Catan map [c] upgraded to a city. Raises:
    [InvalidPlacement] if node at [n] is not currently a settlement
    owned by player [pl]. Requires:[c] is a valid catan map and [n] is a
    valid node. [pl] must correspond to valid player.*)

val get_node_settlement : t -> node_position -> building

val buy_road_init :
  t -> Player.player_id -> node_position * node_position -> t
(** [buy_road_init c pl (n1,n2)] is catan map [c] with [pl] the owner of
    the road between nodes [n1] and [n2]. Raises: [InvalidPlacement] if
    [pl] does not own [n1] or [n2], or if [(n1,n2)] is an invalid road.
    Requires: [c] is a valid catan map and [n1] and [n2] are valid
    nodes. [pl] must correspond to valid player.*)

val buy_road :
  node_position * node_position -> Player.player_id -> t -> t
(** [buy_road (n1,n2) pl c] is catan map [c] with [pl] the owner of the
    road between nodes [n1] and [n2]. Raises: [InvalidPlacement] if [pl]
    does not own [n1] or [n2] or a road adjacent to [n1] or [n2], and
    will also raise [InvaidPlacement]] if edge is already owned.
    Requires: [c] is a valid catan map and [n1] and [n2] are valid
    nodes. [pl] must correspond to valid player.*)

val get_updates :
  roll_number -> t -> (Player.player_id * Player.resource) list
(** [get_updates roll c up] is a list of doubles containing a player and
    the resources that the player should collect given roll [roll] and
    catan map [c]. A player may appear multiple times in [up] if they
    own multiple nodes that collect resources on the same [roll].
    Example: [pl] owns a city on a node that touches a hexagon whose
    roll number is [roll_number] and corresponds to resource [res]. The
    returned list will contain the tuples [(pl, res);(pl, res)]
    Requires: [c] is a valid catan map and
    [roll]={1,2,3,4,5,6,8,9,10,11,12}.*)

val get_node_settlement : t -> node_position -> building
(** [get_node_settlement board pos] is the building that is at a node
    position [pos] on [board]. Requires: [pos] is a valid integer
    between 0 and 52 and [board] is a valid board.*)

val print_board :
  t -> (Player.player_id * ANSITerminal.style) list -> unit
(** [print_board board colors] pretty prints the board [board] with
    colors to cli. Hexagons are labeled with their corresponding roll
    numbers Requires: [board] is a valid board*)

val print_board_alt :
  t -> (Player.player_id * ANSITerminal.style) list -> unit
(** [print_board_alt board colors] pretty prints the board [board] with
    colors to cli. Hexagons are labeled 0-18 Requires: [board] is a
    valid board*)

val get_robber_pos : t -> hex_position
(** [get_robber_pos c] is the hexagon on which the robber currently lies*)

val rob : hex_position -> t -> t
(** [rob h c] is catan board [c] with the hexagon at position [h]
    robbed. All adjacent nodes can no longer collect resources until a
    different hexagon is robbed. Raises [RobberNotMoved] if
    [get_robber_pos c] = [n] (i.e. the robber is not moved to a new
    hexagon). Requires: [h >= 0] and [h <= 18] (h must correspond to
    valid hexagon)*)

val get_player_edges_from_board : Player.t -> t -> edge list
