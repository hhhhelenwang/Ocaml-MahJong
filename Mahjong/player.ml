(** *)
type handt={
  mutable light: Tile.t list;
  mutable dark: Tile.t list;
}

type t ={
  id:int;
  (**State_r tells about state of current player.
     If the player have already riichi,state_r is true
     If the player havn't riichi, state_r state *)
  mutable state_r: bool;

  (**State_c tells about state of current player.
     If the player have done action chii, this state is true
     If the player have not, this state is false*)
  mutable state_c : bool;

  (**the tile player currently have. Number varies from 13 to 14 *)
  (**hsd *)
  hand_tile : handt;

  (**tiles that played by this player*)
  mutable discard_pile : Tile.t list;
}

let p_id t = t.id

let state_r t = t.state_r

let state_c t = t.state_c

let rec check_tile list tid = 
  match list with
  |[] -> None
  |h :: t -> if (tid = Tile.get_id h) then Some h
    else check_tile t tid

let update_pile tid pile = 
  List.filter (fun x -> Tile.get_id x <> tid) pile

let discard_tile player tid =
  let handt = player.hand_tile.dark in
  let discardt = player.discard_pile in 
  match check_tile handt tid with
  | None -> false
  | Some h -> Tile.update_status h;
    player.hand_tile.dark <- update_pile tid handt;
    player.discard_pile <- update_pile tid discardt; 
    true