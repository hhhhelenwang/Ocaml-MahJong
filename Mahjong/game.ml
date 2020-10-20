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

type t = game_state

(** [extract_rand_tile] extracts _one_ random tile from a given list of tiles.
    The function extracts the nth element where [n] = [Random.in len] is a 
    randomly genderated index of the list. [extract_rand_tile] evaluates to a 
    tuple (picked, rest) where picked is the tile that is picked out and rest
    is the list of tiles left.
    Requires: [tiles] is a valid list of tiles that contain the complete the 
    complete set of MahJong tile.
*)
let rec extract_rand_tile 
    (acc : Tile.t list) 
    (n : int) 
    (tiles : Tile.t list) : (Tile.t * Tile.t list) = 
  failwith"TODO"

(** [assign_tone_player] assign tiles to one player. It evaluates to a 
    (list * list) tuple (picked, rest) where [picked] is the list of tiles
    given to this player and [rest] is the list of tiles that have not been
    given out.
    [n_of_tiles]: number of tiles assigned to the player
    [tiles]: the list of tiles from which the player is assigned
    Requires: [tiles] is a valid list of tiles that contain the complete the 
    complete set of MahJong tile.
*)
let rec assign_one_player 
    (n_of_tiles : int)
    (tiles : Tile.t list) : (Tile.t list * Tile.t list) =
  failwith "TODO"

(** [assign_all_players] assigns tiles to all players. It is a list of [n] tile 
    list where [n] = number of players.
    [n_of_plr]: number of players
    [tiles]: the list of the complete set of tiles
    Requires: [tiles] is a valid list of tiles that contain the complete the 
    complete set of MahJong tile.*)
let assign_all_players 
    (n_of_plr : int) 
    (tiles : Tile.t list) : (Tile.t list list) = 
  failwith "TODO"

(** Random allocation of tiles is inplemented by blahblahblah *)
let make_game tiles = 
  failwith "TODO"

(**  *)
let next_state state = 
  failwith "TODO"
