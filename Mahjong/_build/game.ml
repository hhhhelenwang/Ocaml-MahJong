
(** AF: a record 
    {wall_tiles = [Tile1, Tile2, ... , Tilen]; 
    player = [Player1, Player2, Player3, Player];} represents a state of the 
    game with 
    - wall tiles {Tile1, Tile2, ... , Tilen} and 
    - players {Player1, ... ,Player4}.

    RI: none. *)
type game_state = {
  wall_tiles : Tile.t list;
  players : Player.t list;
}

(** Random allocation of tiles is inplemented by blahblahblah *)
let make_game tiles = 
  failwith "TODO"

(**  *)
let next_state state = 
  failwith "TODO"
