(** The type of the id of a tile. *)
type id = int

(** The type of the kind of a tile. *)
type kind = Man | Pin | Sou | Dragon | Wind

(** A record to represent tile*)
type tile = {
  id : id;
  kind : kind;
  number : int;
  mutable discarded : bool;
}

type t = tile

let update_status tile = tile.discarded <- true

let get_id t = t.id

let dp_kind t=
  match t.kind with
  |Man -> print_string "Man   "
  |Pin -> print_string "Pin   "
  |Sou -> print_string "Sou   "
  |Dragon -> print_string "中   "
  |Wind -> print_string "东    "

let dp t=
  dp_kind t;
  ()