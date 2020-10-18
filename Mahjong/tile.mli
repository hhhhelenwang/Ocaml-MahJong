(** 
   Representation of dynamic tile data.

   This module is a representation of each tile, including the kinds, ids, 
   and discardable status.
*)

(** The abstract type of values representing a tile. *)
type t

(** The type of the id of a tile. *)
type id = int

(** Raised when an unknown tile is played. *)
exception UnknownTile of id

(** The discardable status of a tile *)
val update_status : t -> t