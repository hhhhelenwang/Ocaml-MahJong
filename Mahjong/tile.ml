(** The type of the id of a tile. *)
type id = int

(** The type of the kind of a tile. *)
type kind = Man | Pin | Sou | Dragon | Wind

exception UnknownTile of id

(** A record to represent tile*)
type tile = {
  id : id;
  kind : kind;
  number : int;
  mutable discarded : bool;
}

type t = tile

(**make tile discarded *)
let update_status tile = tile.discarded <- true

let get_id t = t.id

let dp t=
  let n= t.number in
  match t.kind with
  | Man -> print_string "   Man "; print_int n
  | Pin -> print_string "   Pin "; print_int n
  | Sou -> print_string "   Sou "; print_int n
  | Wind-> begin match n with
      |1 -> print_string "   East"
      |2 -> print_string "   South"
      |3 -> print_string "   West"
      |4 -> print_string "   North"
      |_ -> print_string "   Not right"
    end
  | Dragon -> begin match n with
      |1 -> print_string "   Red_Dragon"
      |2 -> print_string "   Green_Dragon"
      |3 -> print_string "   White_Draon"
      |_ -> print_string "   Not right"
    end

let construct id kind num b =
  {
    id = id;
    kind = kind;
    number = num;
    discarded = b; 
  }

(**check if it is Nine-one tile *)
let ck_n_o t=
  match t.kind with
  | Dragon | Wind-> true
  | Man | Pin | Sou -> begin
      let n=t.number in
      if (n=1 || n=9) then true
      else false
    end

(*helper function for Rong***********************************************)
(** check if two tile is adjacent*)
let ck_adj t1 t2 =
  match t1.kind with
  | Dragon | Wind -> false
  | Man | Pin | Sou ->
    begin
      if t1.kind = t2.kind then begin
        let k = t2.number - t1.number in
        if (k = 1) then true
        else false
      end
      else false
    end 

(**check if three tiles can form a sequence
    AF: return true for sequence with same kind and sequential number, for 
    example, Man 1 Man 2 Man3 
    RI:  t1 t2 t3 would be in accending order*)
let ck_seq t1 t2 t3=
  (ck_adj t1 t2) && (ck_adj t2 t3)

(**check if two tile is the same *)
let ck_eq t1 t2=
  if t1.kind = t2.kind then begin 
    if t1.number = t2.number then true
    else false
  end
  else false

(** check if three tiles are identical
    AF: return true for three identical tiles, for example,
    Man 1 Man 1 Man 1
    RI:  t1 t2 t3 would have same kind and number
*)
let ck_ke t1 t2 t3= 
  ck_eq t1 t2 && ck_eq t2 t3

(*end of helper function for Rong******************************************)

(* [filter_kind kind lst ] gives all tiles with specific kind *)
let filter_kind kind lst =
  List.filter (fun x -> x.kind == kind) lst

(* [sort_one_kind kind lst] returns the sorted list of one kind. 
   [compare t1 t2] compares the number between tiles*)
let sort_one_kind kind lst = 
  let compare t1 t2 = t1.number - t2.number in
  List.sort compare (filter_kind kind lst)

let sort lst =
  let kinds = [Pin; Man; Sou; Wind; Dragon] in 
  let rec helper acc kinds =
    match kinds with
    | [] -> acc
    | h :: t -> helper (acc @ sort_one_kind h lst) t
  in helper [] kinds