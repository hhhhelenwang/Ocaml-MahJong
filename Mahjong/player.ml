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

let discard_pile tid pile : Tile.t list = (
  List.filter (fun x -> Tile.get_id x <> tid) pile
)

(* [chii player tile] modifies the hand-tile of the player
   put tile in 
*)
let chii player tile = 
  failwith ""

(* 12 choose 3, 9 choose 3, 6 choose 3, 3
   tile 1 $ [[tile2; til3];[tile 4; tile 5]] gives
   [[tile1; tile2; til3]; [tile1; tile4; til5]] *)
let ($) x lst = List.map (fun a -> x::a) lst

let rec extract k lst = 
  if k = 0 then [[]]
  else 
    match lst with 
    | [] -> []
    | h :: t -> let with_h = h $ (extract (k-1) t) in
      let without_h = extract k t in 
      with_h @ without_h

(** keep track of number of tiles each kind and number has*)


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
let rec get_3 n1 n2 tup =
  if n1 = 0 then
    let new_t= { tup with pos = []; long_list= tup.pos::tup.long_list } in
    new_t 
  else
  if n2 = 0 then 
    let new_t = { tup with c_comb = []; 
                           pos = tup.c_comb::tup.pos; 
                           left_list = []; 
                           right_list = tup.left_list } in
    get_3 (n1-1) 3 new_t
  else begin
    match tup.right_list with 
    | [] -> { tup with c_comb = []; 
                       left_list = []; 
                       right_list = tup.left_list @ tup.right_list }
    | h :: t -> begin 
        let new_comb = tup.c_comb @ [h] in 
        if List.length tup.right_list = n2
        then get_3 n1 (n2-1) {tup with c_comb = new_comb;
                                       left_list = tup.left_list @ [h];
                                       right_list = t}
        else if n2 mod 3 = 0
        then get_3 n1 (n2-1) { tup with c_comb = new_comb;
                                        left_list = tup.left_list;
                                        right_list = t }
        else
          let new_tup = get_3 n1 (n2-1) { tup with c_comb = new_comb; 
                                                   right_list = t } in
          get_3 n1 n2 { new_tup with left_list = tup.left_list @ [h]; 
                                     right_list = t }
      end
  end



type  n_comb={
  pair:Tile.t list;
  ke_zi: Tile.t list list;
  info: (Tile.t*int) list;
  rest_tile: Tile.t list;
  seq: Tile.t list list;
  mutable rong: bool;
}

(* find h in acc, if exist >> (h, count+1), if not, append (h,1) on acc *)
let rec generate_info h left_acc right_acc=
  match right_acc with 
  | [] -> left_acc @ [(h,1)]
  | (tile, count) :: t -> begin
      if Tile.ck_eq h tile then left_acc @ [tile, count + 1] @ right_acc
      else generate_info h (left_acc @ [(tile, count)]) t
    end

(* [ini_info lst acc] takes in a list [lst] of tiles and return a list [acc] 
   of tuples [(tile, count)]*)
let rec ini_info lst acc=
  match lst with
  | [] -> acc
  | h :: t -> 
    ini_info t (generate_info h [] acc)

(** [get_info] gets the count of a certain tile*)
let rec get_info h info=
  match info with
  | [] -> 0
  | (tile, count) :: t -> begin
      if Tile.ck_eq h tile then count
      else get_info h t
    end

(* [rem_l int lst] remove first [int] elements from [lst] *)
let rec rem_l int lst=
  if int = 0 then lst 
  else begin
    match lst with
    | [] -> failwith "not right"
    | h :: t -> rem_l (int - 1) t
  end

(* [rem_info_c int h left_acc right_acc] returns a list with 
   removed [int] counts from tuple curresponding to tiile [h]*)
let rec rem_info_c int h left_acc right_acc =
  match right_acc with 
  | [] -> left_acc
  | (tile, count) :: t -> begin
      if Tile.ck_eq h tile then left_acc @ [tile, count - int] @ right_acc
      else rem_info_c int h (left_acc @ [(tile, count)]) t
    end

(* [rem_li_seq n seq lst left_list] remove the first sequence from a sorted lst
    require: seq must be a sequence *)
let rec rem_li_seq n seq lst left_list= 
  if n=0 then left_list @ lst
  else 
    match lst with
    | [] ->failwith "Not right input"
    | h :: t -> 
      begin match seq with 
        | [] ->failwith "Not right input"
        | x :: y -> if (Tile.ck_eq x h)
          then rem_li_seq (n-1) y t left_list
          else rem_li_seq n lst t (left_list @ [h])
      end

(* [remove_info_seq int info left_acc] count-- for first three tuple in info *)
let rec remove_info_seq int info left_acc = 
  if int = 0 then left_acc
  else
    match info with
    | [] -> failwith "not right"
    | (tile, count) :: t -> 
      remove_info_seq (int-1) t (left_acc @ [(tile, count-1)])

(* [remove_zero_count lst] removes all tuples with count = 0 *)
let remove_zero_count lst = 
  List.filter (fun (tile, count) -> count <> 0) lst

(* [get_first_three int info acc] returns the first three tiles in info list*)
let rec get_first_three int (info:(Tile.t*int) list) acc = 
  if int = 0 then acc 
  else begin
    match info with
    | [] -> failwith "not right"
    | (tile, count) :: t -> get_first_three (int-1) t (acc @ [tile])
  end

let check_sequence lst = 
  match lst with
  | t1 :: t2 :: t3 :: []-> Tile.ck_seq t1 t2 t3
  | _ -> failwith "not right input for check sequence"

(* let rec di_gui n_comb = 
   if List.length n_comb.rest_tile = 0 
   then begin 
    if ((2 = List.length n_comb.pair) && (List.length n_comb.ke_zi + List.length n_comb.seq = 4))
    then true else false end
      else 
    match n_comb.rest_tile with
    | [] -> failwith "not right"
    | h :: t -> 
      (* check ke *)
      let count_ke = get_info h n_comb.info in 
      if count_ke > 2 then
        let new_r = rem_l 3 n_comb.rest_tile in
        let new_info = rem_info_c 3 h [] n_comb.info in
        let new_k = [h;h;h] ::n_comb.ke_zi in
        if (di_gui {n_comb with rest_tile=new_r; info=new_info;ke_zi=new_k }) 
        then true
        else begin 
          (* check pair*)
          if (count_ke > 1 && n_comb.pair <> [] ) then 
            let new_r = rem_l 2 n_comb.rest_tile in
            let new_info = rem_info_c 2 h [] n_comb.info in
            let new_p =[h;h] in
            if (di_gui {n_comb with rest_tile=new_r; info=new_info;pair= new_p})
            then true
            else begin
              (* check  sequence*)
              if (check_sequence (get_first_three 3 n_comb.info [])) 
              then 
                let new_r = rem_l 3 n_comb.rest_tile in
                let new_info = rem_info_c 3 h [] n_comb.info in
                let new_s = (get_first_three 3 n_comb.info []) :: n_comb.seq in
                if (di_gui {n_comb with rest_tile=new_r; info=new_info; seq= new_s})
                then true
                else false
              else false
            end
          else false
        end
      else 
        false
*)

(* let rec di_gui n_comb = 
   if List.length n_comb.rest_tile = 0 
   then begin 
    if ((2 = List.length n_comb.pair) && (List.length n_comb.ke_zi + List.length n_comb.seq = 4))
    then n_comb.rong = true else n_comb.rong = false end
   else 
    match n_comb.rest_tile with
    | [] -> failwith "not right"
    | h :: t -> 
      (* check ke *)
      let count_ke = get_info h n_comb.info in 
      if count_ke > 2 then
        let new_r = rem_l 3 n_comb.rest_tile in
        let new_info = rem_info_c 3 h [] n_comb.info in
        let new_k = [h;h;h] ::n_comb.ke_zi in
        if (di_gui {n_comb with rest_tile=new_r; info=new_info;ke_zi=new_k }) 
        then 
        else ()
        (* check pair*)
      else ()

      if (count_ke > 1 && n_comb.pair <> [] ) then 
        let new_r = rem_l 2 n_comb.rest_tile in
        let new_info = rem_info_c 2 h [] n_comb.info in
        let new_p =[h;h] in
        if (di_gui {n_comb with rest_tile=new_r; info=new_info;pair= new_p})
        then n_comb.rong = true
        else begin
          (* check  sequence*)
          if (check_sequence (get_first_three 3 n_comb.info [])) 
          then 
            let new_r = rem_l 3 n_comb.rest_tile in
            let new_info = rem_info_c 3 h [] n_comb.info in
            let new_s = (get_first_three 3 n_comb.info []) :: n_comb.seq in
            if (di_gui {n_comb with rest_tile=new_r; info=new_info; seq= new_s})
            then n_comb.rong = true
            else n_comb.rong = false
          else false
        end
      else false
   end
   else 
   false *)




let rec di_gui n_comb = 

  if List.length n_comb.rest_tile = 0 
  then begin 
    if ((2 = List.length n_comb.pair) && (List.length n_comb.ke_zi + List.length n_comb.seq = 4))
    then true else false end

  else 
    match n_comb.rest_tile with
    | [] -> failwith "not right"
    | h :: t -> 
      (* check ke *)
      let count_ke = get_info h n_comb.info in 
      if count_ke > 2
      then begin
        let new_r = rem_l 3 n_comb.rest_tile in
        let new_info = rem_info_c 3 h [] n_comb.info in
        let new_k = [h;h;h] ::n_comb.ke_zi in 
        if (di_gui {n_comb with rest_tile=new_r; info=new_info;ke_zi=new_k }) 
        then true 
        (** pair*)
        else if (count_ke > 1 && n_comb.pair = [] ) then 
          let new_r = rem_l 2 n_comb.rest_tile in
          let new_info = rem_info_c 2 h [] n_comb.info in
          let new_p =[h;h] in
          begin
            if (di_gui {n_comb with rest_tile=new_r; info=new_info;pair= new_p})
            then true  
            (*chek sequence*)
            else if (check_sequence (get_first_three 3 n_comb.info [])) 
            then 
              let seq= get_first_three 3 n_comb.info [] in
              let new_r = rem_li_seq 3 seq n_comb.rest_tile [] in
              let new_info = rem_info_c 3 h [] n_comb.info in
              let new_s = seq :: n_comb.seq in
              begin if (di_gui {n_comb with rest_tile=new_r; info=new_info; seq= new_s})
                then true 
                else false 
              end
            else false
          end
        else
          (**check sequence*)
        if (check_sequence (get_first_three 3 n_comb.info [])) 
        then 
          let new_r = rem_l 3 n_comb.rest_tile in
          let new_info = rem_info_c 3 h [] n_comb.info in
          let new_s = (get_first_three 3 n_comb.info []) :: n_comb.seq in
          begin if (di_gui {n_comb with rest_tile=new_r; info=new_info; seq= new_s})
            then true 
            else false
          end
        else false
      end
      else
        (*chek pair*)
      if (count_ke > 1 && n_comb.pair = [] ) then 
        let new_r = rem_l 2 n_comb.rest_tile in
        let new_info = rem_info_c 2 h [] n_comb.info in
        let new_p =[h;h] in
        begin
          if (di_gui {n_comb with rest_tile=new_r; info=new_info;pair= new_p})
          then true  
          (*chek sequence*)
          else if (check_sequence (get_first_three 3 n_comb.info [])) 
          then 
            let new_r = rem_l 3 n_comb.rest_tile in
            let new_info = rem_info_c 3 h [] n_comb.info in
            let new_s = (get_first_three 3 n_comb.info []) :: n_comb.seq in
            begin if (di_gui {n_comb with rest_tile=new_r; info=new_info; seq= new_s})
              then true 
              else false 
            end
          else false
        end
      else if (check_sequence (get_first_three 3 n_comb.info [])) 
      then 
        let new_r = rem_l 3 n_comb.rest_tile in
        let new_info = rem_info_c 3 h [] n_comb.info in
        let new_s = (get_first_three 3 n_comb.info []) :: n_comb.seq in
        begin if (di_gui {n_comb with rest_tile=new_r; info=new_info; seq= new_s})
          then true 
          else false 
        end
      else if (check_sequence (get_first_three 3 n_comb.info [])) 
      then 
        let new_r = rem_l 3 n_comb.rest_tile in
        let new_info = rem_info_c 3 h [] n_comb.info in
        let new_s = (get_first_three 3 n_comb.info []) :: n_comb.seq in
        begin if (di_gui {n_comb with rest_tile=new_r; info=new_info; seq= new_s})
          then true 
          else false 
        end
      else false


(* check pair
   else if (count_ke > 1 && n_comb.pair <> [] ) then begin
   let new_r = rem_l 2 n_comb.rest_tile in
   let new_info = rem_info_c 2 h [] n_comb.info in
   let new_p =[h;h] in
   begin
    if (di_gui {n_comb with rest_tile=new_r; info=new_info;pair= new_p})
    then true
    else
      (* check  sequence*)
   else if (check_sequence (get_first_three 3 n_comb.info [])) 
   then begin
   let new_r = rem_l 3 n_comb.rest_tile in
   let new_info = rem_info_c 3 h [] n_comb.info in
   let new_s = (get_first_three 3 n_comb.info []) :: n_comb.seq in
   if (di_gui {n_comb with rest_tile=new_r; info=new_info; seq= new_s})
   then true
   else false
   end
   end

   end *)


let ini_comb lst ={
  pair = [];
  ke_zi = [];
  info = ini_info lst [];
  rest_tile = lst;
  seq = [];
  rong = false;
}


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
   case 12: 123 234 234
            12223344
            11 222 333 4   > 123 123 234
*)

(* let ck_3333 =
   failwith "" *)

(**check if the list fullfill with 4 part *)
let ck_12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 =
  (Tile.ck_seq  x1 x5 x9) && (Tile.ck_seq  x2 x6 x10)
  && (Tile.ck_seq  x3 x7 x11) && (Tile.ck_seq  x4 x8 x12)
