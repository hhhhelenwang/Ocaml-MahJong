(** AF: a record 
    {wall_tiles = [Tile1; Tile2; ... ; Tilen ]; 
    player = [Player1; Player2; Player3; Player4];} represents a state of the 
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
  let a = (id - 1) / 9 in
  let x = 1 + (id mod 9) in
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
  let y = 1 + (id - 81) mod 4 in (Tile.Wind, y)

(* Dragon:
   - id = 124 + c * 3 + z, where id > 124
   - c = 0, 1, 2
   - z = 1, 2, 3
   - Dragon z *)
let dragon_of_id id = 
  let z = 1 + (id - 124) mod 3 in (Tile.Dragon, z)

(* [tile_of_num] is a tile calculated from a given [num]. The relation 
   between a tile and a num is described above. *)
let tile_of_id id = 
  let kind, n = begin
    if id <= 108 then mps_of_id id
    else if id <= 124 then wind_of_id id
    else dragon_of_id id
  end in
  Tile.construct id kind n false

(* [init] initializes a list of something *)
let rec init_tiles id acc = 
  if id = 0 then acc
  else init_tiles (id - 1) (tile_of_id id :: acc)

(** [init_state] is a game state where 
    - [wall_tiles] contains all tiles in the game
    - [players] has each players with an empty list of hand tiles *)
let init_state () = 
  {
    wall_tiles = init_tiles 136 [];
    players = []; (* no players yet *)
  }

(** extracts the nth element from a list *)
let rec extract not_picked n = function
  | [] -> raise Not_found
  | h :: t -> begin
      if n = 0 then (h, not_picked @ t)
      else extract (h :: not_picked) (n - 1) t
    end 

(** [extract_n] extracts n elements and put then in a new list.
    [extract_n] = (extracted, left) *)
let rec extract_n lst n acc op = 
  if n = 0 then (acc, lst)
  else 
    let picked, not_picked = op lst in
    extract_n not_picked (n - 1) (picked :: acc) op

(** [extract_rand] extracts one random element from [lst]. Evaluates to a 
    tuple (picked, not_picked). *)
let extract_rand lst = 
  let n = lst |> List.length |> Random.int in
  extract [] n lst

(** shuffles [lst] *)
let shuffle_list lst = 
  let shuffled, left = extract_n lst (List.length lst) [] extract_rand in
  shuffled

(** [extract_seq] is the op passed into [extract_n] for sequentially extracting 
    the first n elements.*)
let extract_seq lst =
  extract [] 0 lst

(** Extract first n elementes for a list and construct a new list from them.
    Evaluates to (extracted, left) *)
let extract_first_n lst n = 
  extract_n lst n [] extract_seq

(** [make_game] is a game state where 
    - [wall_tiles] is shuffled and first 4 * 13 tiles are given to each player
    - [players] has each player with 13 hand tiles *)
let make_game state = 
  let shuffled_tiles = shuffle_list state.wall_tiles in
  let rec assign n_of_p shuffled_tiles acc = 
    if n_of_p = 0 then acc
    else begin
      let hand, left = extract_first_n shuffled_tiles 13 in
      let player = Player.init_player n_of_p false false [] hand [] in
      assign (n_of_p - 1) left (player :: acc)
    end in
  let players = assign 4 shuffled_tiles [] in
  { state with players = players }

(** [next_state] is the game state after a player has made a move *)
let next_state state = 
  failwith "TODO"

let rec display_all_player players =
  match players with
  | [] -> ()
  | h :: t -> 
    print_string "\n player \n";
    Player.display_I h;
    display_all_player t

let display_game state = 
  display_all_player state.players