type t =
  | Knight
  | RoadBuilding
  | YearOfPlenty
  | Monopoly
  | VictoryPoint

let to_string = function
  | Knight -> "Knight"
  | RoadBuilding -> "Road building"
  | YearOfPlenty -> "Year of plenty"
  | Monopoly -> "Monopoly"
  | VictoryPoint -> "Victory point"

let get_message_from_card = function
  | Knight -> "The knight card allows you to move the robber."
  | RoadBuilding ->
      "The road building card allows you to place two roads on any \
       unoccupied spaces on the map. "
  | YearOfPlenty ->
      "You may draw two resources of any type from the bank."
  | Monopoly ->
      "You claim all resource cards of a specific type from other \
       players."
  | VictoryPoint ->
      "You earn one extra victory point. You may play this card at any \
       point."
