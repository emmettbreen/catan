(** Representation of development cards in the game Catan

    This module contains all of the functions and type definitions for
    development cards, including the type of cards and their effects on
    the game. *)

type t
(** The abstract type of values that represent a development card in
    Catan. *)

val to_string : t -> string
(** [to_string card] pretty prints the content of development card
    [card] Requires: [pl] is a valid player*)

val get_message_from_card : t -> string
(** [get_message_from_card crd] is the output prompt to the player after
    a player receives or uses a development card. *)
