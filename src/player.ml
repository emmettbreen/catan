type player_id = string

type resource =
  | Brick
  | Lumber
  | Ore
  | Grain
  | Wool
  | None

exception InsufficientResources of resource

exception NotAResource

exception InvalidPlacement

type owned_resources = {
  brick : int;
  lumber : int;
  grain : int;
  wool : int;
  ore : int;
}

type t = {
  id : player_id;
  resources : owned_resources;
  victory_points : int;
  color : ANSITerminal.style;
}

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

let get_player_color_text pl =
  match pl.color with
  | ANSITerminal.Foreground color -> (
      match color with
      | ANSITerminal.Green -> "Green"
      | ANSITerminal.Blue -> "Blue"
      | ANSITerminal.Yellow -> "Yellow"
      | ANSITerminal.Red -> "Red"
      | _ -> "None")
  | _ -> "None"

let to_string pl =
  "Player:\n" ^ "\tid: " ^ pl.id ^ "\n\tcolor: "
  ^ get_player_color_text pl
  ^ "\n\tResources:" ^ "\n\t\tBrick: "
  ^ string_of_int pl.resources.brick
  ^ "\n\t\tLumber: "
  ^ string_of_int pl.resources.lumber
  ^ "\n\t\tGrain: "
  ^ string_of_int pl.resources.grain
  ^ "\n\t\tWool: "
  ^ string_of_int pl.resources.wool
  ^ "\n\t\tOre: "
  ^ string_of_int pl.resources.ore

let print_player pl =
  Printers.color_print pl.color (to_string pl);
  print_endline ""

let resource_to_string res =
  match res with
  | Brick -> "Brick"
  | Lumber -> "Lumber"
  | Ore -> "Ore"
  | Grain -> "Grain"
  | Wool -> "Wool"
  | None -> ""

let get_num_resource pl res =
  match res with
  | Brick -> pl.resources.brick
  | Lumber -> pl.resources.lumber
  | Grain -> pl.resources.grain
  | Wool -> pl.resources.wool
  | Ore -> pl.resources.ore
  | _ -> raise NotAResource

let modify_owned_resources resources resource n =
  match resource with
  | Brick -> { resources with brick = resources.brick + n }
  | Lumber -> { resources with lumber = resources.lumber + n }
  | Grain -> { resources with grain = resources.grain + n }
  | Wool -> { resources with wool = resources.wool + n }
  | Ore -> { resources with ore = resources.ore + n }
  | _ -> raise NotAResource

let rec add_resources pl resources =
  match resources with
  | [] -> pl
  | (resource, num) :: t ->
      add_resources
        {
          pl with
          resources = modify_owned_resources pl.resources resource num;
        }
        t

let add_one_resource resources resource =
  match resource with
  | Brick ->
      {
        brick = resources.brick + 1;
        lumber = resources.lumber;
        grain = resources.grain;
        wool = resources.wool;
        ore = resources.ore;
      }
  | Lumber ->
      {
        brick = resources.brick;
        lumber = resources.lumber + 1;
        grain = resources.grain;
        wool = resources.wool;
        ore = resources.ore;
      }
  | Grain ->
      {
        brick = resources.brick;
        lumber = resources.lumber;
        grain = resources.grain + 1;
        wool = resources.wool;
        ore = resources.ore;
      }
  | Wool ->
      {
        brick = resources.brick;
        lumber = resources.lumber;
        grain = resources.grain;
        wool = resources.wool + 1;
        ore = resources.ore;
      }
  | Ore ->
      {
        brick = resources.brick;
        lumber = resources.lumber;
        grain = resources.grain;
        wool = resources.wool;
        ore = resources.ore + 1;
      }
  | None ->
      {
        brick = resources.brick;
        lumber = resources.lumber;
        grain = resources.grain;
        wool = resources.wool;
        ore = resources.ore;
      }

let player_with_new_resource pl resource =
  { pl with resources = add_one_resource pl.resources resource }

let get_num_victory_points pl = pl.victory_points

let dec_num_victory_ponts pl num =
  { pl with victory_points = pl.victory_points - num }

let inc_num_victory_ponts pl num =
  { pl with victory_points = pl.victory_points + num }

let get_player_id pl = pl.id

let create_player id res points color =
  add_resources
    {
      id;
      resources =
        { brick = 0; lumber = 0; grain = 0; wool = 0; ore = 0 };
      victory_points = points;
      color;
    }
    res

let init_player id color = create_player id [] 0 color

let init_demo_player id color =
  create_player id
    [
      (Brick, 999); (Lumber, 999); (Ore, 999); (Grain, 999); (Wool, 999);
    ]
    0 color

let trade_resource resource1 resource2 pl =
  if get_num_resource pl resource1 < 4 then
    raise (InsufficientResources resource1)
  else add_resources pl [ (resource1, -4); (resource2, 1) ]

let trade_resource_with_port pl resource1 resource2 =
  if get_num_resource pl resource1 < 3 then
    raise (InsufficientResources resource1)
  else add_resources pl [ (resource1, -3); (resource2, 1) ]

let buy_settlement_with_resources resources =
  {
    brick =
      (if resources.brick >= 1 then resources.brick - 1
      else raise (InsufficientResources Brick));
    lumber =
      (if resources.lumber >= 1 then resources.lumber - 1
      else raise (InsufficientResources Lumber));
    grain =
      (if resources.grain >= 1 then resources.grain - 1
      else raise (InsufficientResources Grain));
    wool =
      (if resources.wool >= 1 then resources.wool - 1
      else raise (InsufficientResources Wool));
    ore = resources.ore;
  }

let buy_city_with_resources resources =
  {
    brick = resources.brick;
    lumber = resources.lumber;
    grain =
      (if resources.grain >= 2 then resources.grain - 2
      else raise (InsufficientResources Grain));
    wool = resources.wool;
    ore =
      (if resources.ore >= 3 then resources.ore - 3
      else raise (InsufficientResources Ore));
  }

let buy_road_with_resources resources =
  {
    brick =
      (if resources.brick >= 1 then resources.brick - 1
      else raise (InsufficientResources Brick));
    lumber =
      (if resources.lumber >= 1 then resources.lumber - 1
      else raise (InsufficientResources Lumber));
    grain = resources.grain;
    wool = resources.wool;
    ore = resources.ore;
  }

let buy_road_with_resources resources =
  {
    brick = resources.brick - 1;
    lumber = resources.lumber - 1;
    grain = resources.grain;
    wool = resources.wool;
    ore = resources.ore;
  }

let buy_city_with_resources resources =
  {
    brick = resources.brick;
    lumber = resources.lumber;
    grain = resources.grain - 2;
    wool = resources.wool;
    ore = resources.ore - 3;
  }

let buy_settlement pl =
  { pl with resources = buy_settlement_with_resources pl.resources }

let buy_city pl =
  { pl with resources = buy_city_with_resources pl.resources }

let buy_road pl =
  { pl with resources = buy_road_with_resources pl.resources }

let set_color pl color = { pl with color }

let get_color pl = pl.color