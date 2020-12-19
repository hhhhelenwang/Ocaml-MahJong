(** The abstract type of values representing player. *)
type t

(** p_id is the identifier of different player.
    Different player should have different id*)
type id = int

(** get the id of this player *)
val p_id : t -> id

(** riichi state of player *)
val state_r : t -> bool

(** chii state of player *)
val state_c : t -> bool

(** get all the light hand tiles of the player *)
val hand_tile_light : t -> Tile.t list

(** get all the dark hand tiles of the player *)
val hand_tile_dark : t -> Tile.t list

(** the list of discarded tiles of the player *)
val discard_pile : t -> Tile.t list

(** [draw_tile] puts tile into the player's dark tiles. *)
val draw_tile : t -> Tile.t -> unit

(** play a tile with id in t, return false if the tile that player wants to
    discard is not in their hand tiles.  *)
val discard_tile: t -> Tile.t option -> bool

(** riichi check if the player can riichi *)
val check_riichi: t -> bool

(** change status from normal to riichi *)
val riichi: t -> unit

(**displayer a player's handtile *)
val display_I : t -> unit

(** display the tiles of current player *)
val d_list: Tile.t list -> unit

(** constructor for a player *)
val init_player: int -> bool -> bool -> Tile.t list -> Tile.t list -> 
  Tile.t list -> t

type combination ={
  left_list: Tile.t list;
  right_list: Tile.t list;
  c_comb: Tile.t list;
  pos : Tile.t list list;
  long_list:  Tile.t list list list;
}

val get_3 : int -> int -> combination -> combination

type  n_comb={
  pair:Tile.t list;
  triplet: Tile.t list list;
  info: (Tile.t*int) list;
  rest_tile: Tile.t list;
  seq: Tile.t list list;
  mutable rong: bool;
}

val ron_legal : n_comb -> bool
val ini_comb: Tile.t list -> n_comb
val check_triplet :  n_comb -> bool

(** [chii_update_handtile n tile player] updates player's handtile *)
val chii_update_handtile : int -> Tile.t -> t -> unit