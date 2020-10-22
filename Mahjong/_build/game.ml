open Tile
open Player

(** AF: a record 
    {wall_tiles = [| Tile1, Tile2, ... , Tilen |]; 
    player = [Player1, Player2, Player3, Player];} represents a state of the 
    game with 
    - wall tiles {Tile1, Tile2, ... , Tilen} and 
    - players {Player1, ... ,Player4}.

    RI: none. *)
type game_state = {
  (** [wall_tiles] is the deck of unassigned tiles from which players draw. 
      [wall_tiles] contains all the tiles before the game starts. *)
  wall_tiles : Tile.t array;
  (** a list of players *)
  players : Player.t list;
}

type t = game_state

(** [init_state] is a game state where 
    - [wall_tiles] contains all tiles in the game
    - [players] has each players with an empty list of hand tiles *)
let init_state player tile = 
  failwith "TODO"

(** [make_game] is a game state where 
    - [wall_tiles] is shuffled and first 4 * 13 tiles are given to each player
    - [players] has each player with 13 hand tiles *)
let make_game state = 
  failwith "TODO"

(** [next_state] is the game state after a player has made a move *)
let next_state state = 
  failwith "TODO"
