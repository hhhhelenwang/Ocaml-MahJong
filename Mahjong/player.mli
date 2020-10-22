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