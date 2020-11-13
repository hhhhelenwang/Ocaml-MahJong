type id =int
(**type of hand tile *)
type handt={
  mutable light: Tile.t list;
  mutable dark: Tile.t list;
}

type player = {

  (** id of the player*)
  id : int;

  (**State_r tells about state of current player.
     If the player have already riichi,state_r is true, otherwise false *)
  mutable state_r : bool;

  (**State_c tells about state of current player.
     If the player have done action chii, this state is true, otherwise false*)
  mutable state_c : bool;

  (**the tile player currently have. Number varies from 13 to 14 *)
  (**hsd *)
  hand_tile : handt;

  (**tiles that played by this player*)
  mutable discard_pile : Tile.t list;
}

type t = player

let p_id t = t.id

let state_r t = t.state_r

let state_c t = t.state_c

let rec check_tile list tid = 
  match list with
  |[] -> None
  |h :: t -> if (tid = Tile.get_id h) then Some h
    else check_tile t tid

let update_pile tid pile : Tile.t list = (
  List.filter (fun x -> Tile.get_id x <> tid) pile
)

let discard_tile player tid =
  let handt = player.hand_tile.dark in
  let discardt = player.discard_pile in 
  match check_tile handt tid with
  | None -> false
  | Some h -> begin 
      Tile.update_status h;
      player.hand_tile.dark <- update_pile tid handt;
      player.discard_pile <- update_pile tid discardt;
      true
    end

let d_list l = 
  List.iter Tile.dp l

let display_I t = 
  let hand = t.hand_tile in 
  let lt = hand.light in 
  let dt = hand.dark in
  d_list lt;
  d_list dt

let riichi t=
  failwith "hah"

let check_riichi t=
  failwith "hah"

(** [init_player id richii chii light dark discard] is the constructor of 
    player type*)
let init_player id richii chii light dark discard =
  let handt = 
    {
      light = light;
      dark = dark;
    } in {
    id = id;
    state_r = richii;
    state_c = chii;
    hand_tile = handt;
    discard_pile = discard;
  }

(* [combine t] returns a list of tiles that is the combination of 
   light and dark *)
let combine t = 
  let hand = t.hand_tile in 
  Tile.sort hand.light @ hand.dark

(* [agari t] checks if user can agari or not*)
let agari t =
  let lst= combine t in
  if (List.length lst <> 14) then false
  else 
    failwith ""

(* [get_ele lst n] gets the nth tile in user's combined hand tile *)
let rec get_ele lst n=
  match lst with
  | [] -> []
  | h :: t -> begin
      if n = 0 then h
      else get_ele lst (n-1)
    end

(* [get_ele_range lst n l acc] gets a list of tiles starting from n-th to 
   (n+l)th in user's combined hand tile *)
let rec get_ele_range lst n l acc = 
  if l = 0 then acc
  else get_ele_range lst n (l-1) (get_ele lst (n+1)) @ acc

(* check if user is able to chii. *)
let chii_legal lst tile = 
  (* let temp_lst = Tile.sort [tile] :: lst in  *)
  (* let same_kind = Tile.sort_one_kind tile.kind lst in
     let same_number = Tile.sort_one_number tile.number same_kind in *)
  (* filter out all with same kind and number *)
  (* filter out all with sequencial number and same kind *)
  failwith ""

(* chii *)
let chii tile = 
  failwith ""

(* tile 1 $ [[tile2; til3];[tile 4; tile 5]] gives
   [[tile1; tile2; til3]; [tile1; tile4; til5]] *)
let ($) x lst = List.map (fun a -> x::a) lst

(* given a list of tiles, get all possible ke (Tile.ck_ke) *)
let ke lst = 
  let rec helper n acc add =
    match lst with
    | [] -> acc
    | h :: t -> 
      begin if n == 0 then helper 3 (acc @ (h :: add)) [] 
        else helper n acc add end
  in helper 3 []

let ron lst acc = 
  (* either in ke or in sequence *)
  (* get all possible combinations of ke tile list list*)
  (* check if rest can form sequence *)
  match lst with 
  | [] -> acc
  | 
    failwith ""

(** *)
(** for ( 12 ) ways to seperate list, check if the choosen 3 form seq or ke, 
        ( 3  ) *)

(** [sep_list] produces a list of every possible combinations of handtiles
    lst is list of hand tile
    n is the size of lst
    listlist is in the form of [lst; (Man1, Sou 2, Sou 3); (Man2, Man 2, Man2)]::[]
*)
(* let get_3_list lst n tup_list=
   for i = 0 to n do
    begin for x = i to n do
        begin for y = x to n do
            begin if(i<>x && x<>y && i<> y)then 
                let x1 = get_ele lst i in 
                let x2 = get_ele lst x in
                let x3 = get_ele lst y in
                [x1;x2;x3]::tup_list
            end
          done
        end
      done
    end
   done *)  



(* let rec permutation l r = 
   match r with 
   | [] -> [[]]
   | [x] -> x $ (permutation [] l)
   | h :: t -> 
    let set = permutation (h::l) t in
    (h $ (permutation [] (l@t))) @ set *)

(* n choose 3, n= 12, 12-3, 12-2*3, ... 0(base case) *)
(* let rec choose lst n acc= 
   match lst, (List.length lst)discard_tile with
   | _, size when n == size -> [lst] 
   | h :: t, _ -> (List.map (fun x -> h :: x) (choose t (n-1) )) @ (choose t n)
   | [], _ -> []
               failwith "" *)

type combination= {
  left_list: Tile.t list;
  right_list: Tile.t list;
  c_comb: Tile.t list;
  pos : Tile.t list list;
  long_list:  Tile.t list list list;
}

(**all possibility that divdes 12 tile to 4*3 part *)
let rec get_3 n1 n2 tup=
  if n1=0 then
    let new_t= {tup with pos=[]; long_list= tup.pos :: long_list} in
    new_tup 
  else
  if n2=0 then 
    let new_t= {tup with c_comb=[]; pos= tup.c_comb::tup.pos; left_list=[]; right_list= tup.left_list} in
    get_3 (n1-1) 3 new_t
  else begin
    match tup.right_list with 
    |[] -> {tup with c_comb=[]; left_list=[]; right_list=tup.left_list@tup.right_list}
    |h::t -> let new_comb= tup.c_comb @[h] in 
      begin if (List.length tup.right_list=n2)
        then get_3 n1 (n2-1) {tup with c_comb = new_comb; left_list=tup.left_list @[h]; right_list=t }
        else if 0 = (n2 mod 3)
        then get_3 n1 (n2-1) {tup with c_comb = new_comb; left_list=tup.left_list @[h]; right_list=t }
        else
          let new_tup= (get_3 n1 (n2-1) {tup with c_comb = new_comb; right_list=t }) in
          get_3 n1 n2 {new_tup with left_list= tup.left_list @[h]; right_list=t }
      end
  end




(**case 1: 111 333 555 777 [normal]   3 3 3 3 
   case 2: 223344 567 789          6 3 3
   case 6: 234 556 677 789          3 6 3
   case 7: 234 567 778899          3 3 6
   case 3: 123 333 567 789 [normal]
   case 4: 333344445555        12
   case 5: 222333444 [normal]
   case 8: 133 333
   case 9: 56 666 7  x1 x2 x6
   case 10: 123 455556 789
   case 11: 123 234 x1x2x4
*)

let ck_3333 =
  failwith ""

(**check if the list fullfill with 4 part *)
let ck_12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 =
  (Tile.ck_seq  x1 x5 x9) && (Tile.ck_seq  x2 x6 x10)
  && (Tile.ck_seq  x3 x7 x11) && (Tile.ck_seq  x4 x8 x12)
