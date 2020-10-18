(** 
   Representation of dynamic tile data.

   This module is a representation of each tile, including the kinds, ids, 
   and discardable status.
*)

(** The abstract type of values representing a tile. *)
type t

(** The type of the kind of a tile. *)
type kind = Man | Pin | Sou | Dragon | Wind

(** The type of the number of a tile. *)
type number = int

(** The type of the id of a tile. *)
type id = int

(** The discardable status of a tile *)
type discarded = bool

(** Raised when an unknown tile is played. *)
exception UnknownTile of id

(** The discardable status of a tile *)
val update_status : discarded -> discarded