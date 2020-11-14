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

(**check if three tiles can form a sequence *)
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
*)
let ck_ke t1 t2 t3= 
  ck_eq t1 t2 && ck_eq t2 t3

(*end of helper function for Rong******************************************)

(* [filter_kind kind lst ] gives all tiles with specific kind *)
let filter_kind kind lst =
  List.filter (fun x -> x.kind == kind) lst

(* [sort_one_kind kind lst] returns the sorted list of one kind. 
   [compare t1 t2] compares the number between tiles*)
let sorted_one_kind kind lst = 
  let compare t1 t2 = t1.number - t2.number in
  List.sort compare (filter_kind kind lst)

(* [sort_one_number num lst] returns a list of tiles with same 
   number*)
let sort_one_number num lst = 
  List.filter (fun x -> x.number == num) lst

let sort lst =
  let kinds = [Pin; Man; Sou; Wind; Dragon] in 
  let rec helper acc kinds =
    match kinds with
    | [] -> acc
    | h :: t -> helper (acc @ sorted_one_kind h lst) t
  in helper [] kinds

(* [get_seq f lst num acc] given a number, find if sequence satisfying f 
   exists*)
let rec get_seq f lst num acc = 
  match lst with
  | [] -> begin if List.length acc == 3 then acc else [] end
  | h :: t -> begin
      if f h num
      then get_seq f t num 
          (List.sort_uniq (fun x y -> x.number - y.number) (h::acc))
      else get_seq f t num acc
    end

(* [seq_all lst num] returns all possible sequence. Return empty list if there 
   is no sequence 
   lst is a list with same kind*)
let seq_all lst num = 
  let compare1 h num = h.number >= num -2 && h.number <= num in
  let compare2 h num = h.number >= num && h.number <= num + 2 in 
  let compare3 h num = h.number >= num -1 && h.number <= num + 1 in 
  let compare = [compare1; compare2; compare3] in
  let rec helper acc compare =
    match compare with 
    | [] -> acc
    | h :: t -> helper (
        begin
          if List.length (get_seq h lst num []) = 0 then acc
          else (get_seq h lst num []) :: acc
        end) t
  in helper [] compare

(* get first n element in a list *)
let rec get_first_int lst int acc = 
  if int = 0 then acc else
    match lst with
    | [] -> failwith "wrong list, length < 3"
    | h :: t -> get_first_int t (int-1) (h :: acc)

(* return all possible ke. *)
let pos_ke same_kind t =
  let same_num = sort_one_number t.number same_kind in
  if (List.length same_num > 2) 
  then get_first_int same_num 3 []
  else []

(* return possible ke and seq. Given player is legal to chii. 
   [lst] is player's current dark hand tile, and [t] is the tile 
   we want to check *)
let all_pos lst t =
  let same_kind = sorted_one_kind t.kind (t::lst) in
  let seq_all = seq_all same_kind t.number in
  (pos_ke same_kind t) :: seq_all 

(* [chii_legal lst t] checks if user is able to chii. [lst] is
   player's current dark hand tile, and [t] is the tile we want to
   check *)
let chii_legal lst t = 
  let same_kind = sorted_one_kind t.kind (t::lst) in
  let same_num = sort_one_number t.number same_kind in
  let seq_all = seq_all same_kind t.number in
  List.length same_num > 2 || List.length seq_all > 0

(* [pong_legal lst t] checks is user is able to pong*)
let pong lst t = 
  let same_kind = sorted_one_kind t.kind lst in
  let same_num = sort_one_number t.number same_kind in
  List.length same_num > 2

(* [kong_legal lst t checks if user is able to kong] *)
let kong_legal lst t = 
  let same_kind = sorted_one_kind t.kind lst in
  let same_num = sort_one_number t.number same_kind in
  List.length same_num > 3