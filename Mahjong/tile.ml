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

(** *)
let ck_adj t1 t2 =
  match t1.kind with
  |Dragon |Wind -> 0
  |Man |Pin | Sou ->
    begin
      if (t1.kind=t2.kind)then begin
        let k = t2.number-t1.number in
        if (k=1 ||k = -1 ) then k
        else 0
      end
      else 0
    end 
let ck_eq t1 t2=
  if (t1.kind=t2.kind)then begin 
    if (t1.number=t2.number) then true
    else false
  end
  else false
