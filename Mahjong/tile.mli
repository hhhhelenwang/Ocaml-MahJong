(** 
   Representation of dynamic tile data.

   This module is a representation of each tile, including the kinds, ids, 
   and discardable status.
*)

(** The abstract type of values representing a tile. *)
type t

(** The type of the id of a tile. *)
type id = int

type kind = Man | Pin | Sou | Dragon | Wind

(** Raised when an unknown tile is played. *)
exception UnknownTile of id

(** The discardable status of a tile *)
val update_status : t -> unit

(**get id of this tile *)
val get_id : t -> id

(**display this tile*)
val dp : t -> unit

val construct : id -> kind -> int -> bool -> t