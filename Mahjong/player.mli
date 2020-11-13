(** The abstract type of values representing player. *)
type t

(** p_id is the identifier of different player.
    Different player should have different id*)
type id = int

(**get the id of this player*)
val p_id : t -> id

(** riichi state of player*)
val state_r : t -> bool

(** chii state of player*)
val state_c : t -> bool

(** play a tile with id in t, return exception UnknownTile if id is invalid*)
val discard_tile: t -> Tile.id -> bool

(** riichi check if the player can riichi *)
val check_riichi: t -> bool

(** change status from normal to riichi *)
val riichi: t -> unit

(**displayer a player's handtile*)
val display_I : t -> unit

(** display the tiles of current player*)
val d_list: Tile.t list -> unit

(** constructor for a player*)
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