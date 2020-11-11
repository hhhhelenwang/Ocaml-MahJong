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

let combine t = 
  let hand = t.hand_tile in 
  Tile.sort hand.light @ hand.dark

let agari t =
  let lst= combine t in
  if (List.length lst <> 14) then false
  else 
    failwith ""



let rec get_ele lst n=
  match lst with
  | [] -> []
  | h :: t -> begin
      if n = 0 then h
      else get_ele lst (n-1)
    end

(* let rec get_element_range lst n l acc = 
   if l = 0 then acc
   else get_element_range lst n (l-1) (get_ele lst (n+1)) @ acc
         failwith "" *)


(**case 1: 111 333 555 777 [normal]   3 3 3 3 
   case 2: 223344 567 789          6 3 3
   case 6: 234 556677 789          3 6 3
   case 7: 234 567 778899          3 3 6
   case 3: 123 333 567 789 [normal]
   case 4: 333344445555        12
   case 5: 222333444 [normal]
*)

let ck_3333 =
  failwith ""


let ck_12  x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12=
  (Tile.ck_seq  x1 x5 x9)
