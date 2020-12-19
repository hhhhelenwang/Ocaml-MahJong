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

(** get id of this tile *)
val get_id : t -> id

(** [find_tile kind number] finds the tile represented by [kind] [number]. 
    i.e. given the kind of the tile and the number on the tile, find the 
    complete tile representation among a given list of tiles. In case of 
    duplicated kind and number, find the first occurence.
    [find_tile kind number] = [Some tile] if found, [None] if not found. *)
val find_tile : kind -> int -> t list -> t option

(** remove a tile from a given list.  *)
val remove_tile : t -> t list -> t list

(** display this tile*)
val dp : t -> unit

(** [string_of_tile tile] is a tring representation of [tile]. *)
val string_of_tile : t -> string

(** init a tile *)
val construct : id -> kind -> int -> bool -> t

(** check if it is Nine-one tile *)
val ck_n_o : t -> bool

(**check if two tile are adjasent
   means that it has potential to be a sequence
   return 1 if is adjasent and the latter is the after
   return -1 if is adjasent and later is fower
   return 0 if not adjasent*)
val ck_adj : t -> t -> bool

(** check if two tile is same kind and same number*)
val ck_eq : t -> t -> bool

val ck_seq: t -> t -> t-> bool

(** check if three tile has same kind and number*)
val ck_ke : t -> t -> t-> bool

(**[sort lst] sorts a list of tiles based on kind and number*)
val sort : t list -> t list

(** [sorted_one_kind] returns a list of tiles with same kind*)
(* val sort_one_kind : kind -> t list -> t list *)

(** [sort_one_number] returns a list of tiles with same number*)
(* val sort_one_number : int -> t list -> t list *)

(** [chii_legal lst t] checks if user is able to chii *)
val chii_legal : t list -> t -> bool

(* for testing purpose *)
(* val seq_all : t list -> int -> t  list *)

(* return all possible ke and seq *)
val all_pos : t list -> t -> t list list

val string_tile : t -> string
