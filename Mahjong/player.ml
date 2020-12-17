type id = int

(** type of hand tile *)
type handt = {
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

let hand_tile_light t = t.hand_tile.light

let hand_tile_dark t = t.hand_tile.dark

let discard_pile t = t.discard_pile

let draw_tile player tile =
  player.hand_tile.dark <- [tile] @ player.hand_tile.dark

let discard_tile player tile_opt =
  let handt = player.hand_tile.dark in
  let discardt = player.discard_pile in 
  match tile_opt with
  | None -> false
  | Some tile -> begin 
      Tile.update_status tile;
      player.hand_tile.dark <- Tile.remove_tile tile handt;
      player.discard_pile <- [tile] @ discardt;
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

(* remove [int] number of tiles that are equal to [t] from 
   a list of tiles [right_acc] *)
let rec remove_helper tile left_acc right_acc int = 
  if int = 0 then left_acc @ right_acc
  else begin
    match right_acc with
    | [] -> failwith "wron_legal input"
    | h :: t -> begin
        if Tile.ck_eq h tile then remove_helper tile left_acc t (int -1) 
        else remove_helper tile (h :: left_acc) t (int - 1)
      end
  end

(* remove a list of tiles [tlist] from pile *)
let rec remove_tile tlist pile = 
  match tlist with
  | [] -> pile
  | tile :: t -> remove_tile t (remove_helper tile [] pile 1)


(* given [player] wants to and is eligible to chii tiles [t1] [t2] [t3], 
   update player's handtile*)
let chii_update_handtile int tile player = 
  let hand_tile = player.hand_tile in
  let dark = hand_tile.dark in 
  let light = hand_tile.light in
  let all_pos = Tile.all_pos dark tile in 
  let picked = get_ele all_pos int in
  hand_tile.dark <- picked @ dark;
  hand_tile.light <- remove_tile picked light;
  player.state_c <- true;
  ()

(* let chii_update_handtile t1 t2 t3 player = 
   let hand_tile = player.hand_tile in
   let dark = hand_tile.dark in 
   let light = hand_tile.light in
   hand_tile.dark <- [t1; t2; t3] @ dark;
   hand_tile.light <-  remove_tile [t1; t2; t3] light;
   ()
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
   end *)

type comb = {
  pair: Tile.t list;
  triplet: Tile.t list list;
  info: (Tile.t * int) list;
  (* rest_tile: Tile.t list; *)
  seq: Tile.t list list;
  mutable ron: bool;
}


(* find h in acc, if exist >> (h, count+1), if not, append (h,1) on acc *)
let rec generate_info h left_acc right_acc=
  match right_acc with 
  | [] -> left_acc @ [(h,1)]
  | (tile, count) :: t -> begin
      if Tile.ck_eq h tile then left_acc @ [tile, count + 1] @ t
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

(** update_info clears out tile from info with count = 0 *)
let update_info info = 
  let rec update info acc = 
    match info with
    | [] -> acc 
    | (tile, int) :: t -> begin
        if int <= 0 then update t acc
        else update t (acc @ [(tile, int)])
      end
  in
  update info []

(* [rem_l int lst] remove first [int] elements from [lst] *)
let rec rem_l int lst=
  if int = 0 then lst 
  else begin
    match lst with
    | [] -> failwith "not right rem_l"
    | h :: t -> rem_l (int - 1) t
  end

(* [rem_info_c int h left_acc right_acc] returns a list with 
   removed [int] counts from tuple curresponding to tiile [h]*)
let rec rem_info_c int h left_acc right_acc =
  match right_acc with 
  | [] -> left_acc
  | (tile, count) :: t -> begin
      if Tile.ck_eq h tile then left_acc @ [tile, count - int] @ t
      else rem_info_c int h (left_acc @ [(tile, count)]) t
    end

(* [rem_li_seq n seq lst left_list] remove the first sequence from a sorted lst
    require: seq must be a sequence *)
let rec rem_li_seq n seq lst left_list= 
  if n = 0 then left_list @ lst
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

(* [remove_zero_count lst] removes all tuples with count = 0 *)
let remove_zero_count lst = 
  List.filter (fun (tile, count) -> count <> 0) lst

(* [remove_info_seq int info left_acc] count-- for first three tuple in info *)
let rec remove_info_seq int info left_acc = 
  if int = 0 then (remove_zero_count left_acc @ info)
  else begin
    match info with
    | [] -> failwith "not right remove info seq"
    | (tile, count) :: t -> 
      remove_info_seq (int - 1) t (left_acc @ [(tile, count - 1)])
  end

(* [get_first_three int info acc] returns the first three tiles in info list*)
let rec get_first_three int (info : (Tile.t * int) list) acc = 
  if int = 0 then acc 
  else begin
    match info with
    | [] -> failwith "not right get_first_3"
    | (tile, count) :: t -> get_first_three (int - 1) t (acc @ [tile])
  end

let check_sequence lst = 
  match lst with
  | t1 :: t2 :: t3 :: []-> Tile.ck_seq t1 t2 t3
  | _ -> failwith "not right input for check sequence"

let rec print_info info = 
  match info with
  | [] -> ()
  | (tile, int) :: t -> Tile.dp tile; print_endline (" " ^ string_of_int int);
    print_info t

(**  *)
let rec check_triplet comb = 
  if List.length comb.info = 0 
  then begin 
    if (
      ((List.length comb.pair = 2) &&
       (List.length comb.triplet + List.length comb.seq = 4)) || 
      ((List.length comb.pair = 14) &&
       (List.length comb.triplet + List.length comb.seq = 0))
    )
    then true else false 
  end
  else begin
    match comb.info with
    | [] -> failwith "not right triple"
    | h :: t -> print_endline ("enter check_trip");
      let new_tile = fst h in
      let new_info = update_info (rem_info_c 3 new_tile [] comb.info) in
      let new_k = [new_tile; new_tile; new_tile] :: comb.triplet in
      print_info (comb.info);

      if ( get_info new_tile comb.info > 2 && 
           check_triplet {comb with info = new_info; triplet = new_k}) 
      then true
      else 
        check_pair comb
  end
and 
  check_pair comb = 
  match comb.info with 
  | [] -> failwith "not right pair"
  | h :: t ->  print_endline ("enter check_pair");
    let new_tile = fst h in
    let new_info = update_info (rem_info_c 2 new_tile [] comb.info) in
    let new_p =[new_tile; new_tile] @ comb.pair in begin
      if (get_info new_tile comb.info > 1 
          && check_triplet {comb with info = new_info; pair = new_p})
      then true
      else check_seq comb
    end
and  
  check_seq comb = 
  match comb.info with 
  | [] -> failwith "not right seq"
  | h :: t ->  print_endline ("enter check_comb");
    if List.length comb.info > 2 then
      begin
        let new_info = update_info (remove_info_seq 3 comb.info []) in
        let new_s = (get_first_three 3 comb.info []) :: comb.seq in
        if (check_sequence (get_first_three 3 comb.info []) &&
            check_triplet {comb with info = new_info; seq = new_s})
        then true
        else false 
      end
    else false

let ini_comb lst = {
  pair = [];
  triplet = [];
  info = ini_info lst [];
  (* rest_tile = lst; *)
  seq = [];
  ron = false;
}

let riichi t=
  failwith "hah"



(**hand represent hand tile, list represent various tile type,
   acc represent feasible riichi tile *)
let rec check_r_help hand lst acc = 
  match lst with
  | [] -> acc
  | h ::t -> begin
      let n_hand = Tile.sort (h :: hand) in 
      let n_comb= ini_comb n_hand in
      if (check_triplet n_comb)then
        check_r_help hand t (h :: acc)
      else 
        check_r_help hand t acc
    end

(** given the kind, generate 1-9 tile of this kind  *)
let rec generate_n kind n acc = 
  let new_t= Tile.construct 1 kind n false in
  generate_n kind (n - 1) (new_t :: acc)

(**generate a list of all differnet tile*)
let generate_tiles = 
  let rec generate lst acc = 
    match lst with
    | [] -> acc
    | h :: t -> begin
        match h with
        | Tile.Pin | Tile.Man | Tile.Sou -> generate t (acc @ generate_n h 9 [])
        | Tile.Wind -> generate t (acc @ generate_n h 4 [])
        | Tile.Dragon -> generate t (acc @ generate_n h 3 [])
      end
  in generate [Tile.Pin; Tile.Man; Tile.Sou; Tile.Wind; Tile.Dragon] []

(** return a list of tile that the player needs to richii *)
let check_riichi player =
  if (player.state_c || player.state_r) then []
  else
    let handtile = combine player in
    let lst = generate_tiles in
    check_r_help handtile lst []

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
