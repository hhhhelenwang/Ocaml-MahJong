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

(**init a tile *)
val construct : id -> kind -> int -> bool -> t

(**check if it is Nine-one tile *)
val ck_n_o : t -> bool

(**check if two tile are adjasent
   means that it has potential to be a sequence
   return 1 if is adjasent and the latter is the after
   return -1 if is adjasent and later is fower
   return 0 if not adjasent*)
val ck_adj : t -> t -> bool

(**check if two tile is same kind and same number*)
val ck_eq : t -> t -> bool

(**[sort lst] sorts a list of tiles based on kind and number*)
val sort : t list -> t list