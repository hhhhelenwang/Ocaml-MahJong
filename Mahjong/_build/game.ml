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
  wall_tiles : Tile.t list;
  (** a list of players *)
  players : Player.t list;
}

type t = game_state 

(* Man, Pin, Sou:
   - represented by id = a * 9 + x, where id <= 108
   - if a = 0, 3, 6, 9 => Man
   - if a = 1, 4, 7, 10 => Pin
   - if a = 2, 5, 8, 11 => Sou
   - x + 1 = number on tile
   - e.g. id = 3 = 0 * 0 + 3 => Man 3 *)
let mps_of_id id =
  let a = id - 1 / 9 in
  let x = id mod 9 in
  if a = 0 || a = 3 || a = 6 || a = 9 then (Tile.Man, x)
  else if a = 1 || a = 4 || a = 7 || a = 10 then (Tile.Pin, x)
  else (Tile.Sou, x)

(*  Wind:
    - represented by id = 81 + b * 4 + y, where id > 108
    - b = 0, 1, 2, 3
    - y = 1, 2, 3, 4
    - y = 1 => Wind 1 => East Wind
    - y = 2 => Wind 2 => South Wind
    - y = 3 => Wind 3 => West Wind
    - y = 4 => Wind 4 => North Wind 
    - e.g. id = 82 = 0 * 4 + 1 + 81 => Wind 1 => East Wind *)
let wind_of_id id =
  let y = id - 81 mod 4 in (Tile.Wind, y)

(* Dragon:
   - id = 124 + c * 3 + z, where id > 124
   - c = 0, 1, 2
   - z = 1, 2, 3
   - Dragon z *)
let dragon_of_id id = 
  let z = id - 124 mod 3 in (Tile.Dragon, z)

(* [tile_of_num] is a tile calculated from a given [num]. The relation 
   between a tile and a num is described above. *)
let tile_of_id id = 
  let kind, n = begin
    if id <= 81 then mps_of_id id
    else if id <= 124 then wind_of_id id
    else dragon_of_id id
  end in
  Tile.construct id kind n false

(* [init] initializes a list of something *)
let rec init id acc op = 
  if id = 0 then acc
  else init (id - 1) (op id :: acc) op

(** [init_tiles] is a list containing all tiles, not shuffled *)
let init_tiles id = 
  init id [] tile_of_id

let init_players id = 
  init id [] 

(**[init_state] is a game state where 
   - [wall_tiles] contains all tiles in the game
   - [players] has each players with an empty list of hand tiles *)
let init_state n_of_t n_of_p = 
  failwith


(* [shuffle_array] is the shuffled array of [a]. *)
let shuffle_array a = 
  failwith

(** [make_game] is a game state where 
    - [wall_tiles] is shuffled and first 4 * 13 tiles are given to each player
    - [players] has each player with 13 hand tiles *)
let make_game state = 
  failwith"TODO"


(** [next_state] is the game state after a player has made a move *)
let next_state state = 
  failwith "TODO"
