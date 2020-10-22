type t ={
  id:int;
  (**State_r tells about state of current player.
   If the player have already riichi,state_r is true
   If the player havn't riichi, state_r state *)
  state_r: bool;

  (**State_c tells about state of current player.
   If the player have done action chii, this state is true
   If the player have not, this state is false*)
  state_c : bool;

  (**the tile player currently have. Number varies from 13 to 14 *)
  hand_tile : Tile.t list;

  (**tiles that played by this player*)
  discard_pile : Tile.t list;
}

let p_id t = t.id

let state_r t = t.state_r

let state_c t = t.state_c

let discard_tile t tid =
