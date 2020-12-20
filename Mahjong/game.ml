(** AF: a record 
    {wall_tiles = [Tile1; Tile2; ... ; Tilen ]; 
    player = [Player1; Player2; Player3; Player4];
    current_player_id = n;} represents a state of the 
    game with 
    - wall tiles {Tile1, Tile2, ... , Tilen} and 
    - players {Player1, ... ,Player4}.
    - current player [Player n]

    RI: [wall_tiles] must be valid tiles initialized by a tile constructor.
        [players] must contain 4 players. [current_player_id] is 1, 2, 3, or 4. 
*)
type game_state = {
  (** [wall_tiles] is the deck of unassigned tiles from which players draw. 
      [wall_tiles] contains all the tiles before the game starts. *)
  wall_tiles : Tile.t list;
  (** a list of players *)
  players : Player.t list;
  current_player_id : Player.id;
  in_game : bool
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
    current_player_id = 1;
    in_game = true;
  }

(** [extract not_picked n lst] extracts the [n]th element from [lst] and get 
    the list with remaining elements after extraction. This cannot be done with
    [List.nth] or [List.nth_opt] since the remaining list is needed to ensure
    that no dupicated elements are extracted when extracting multiple 
    elements. *)
let rec extract not_picked n lst = 
  (* match lst with
     | [] -> raise Not_found
     | h :: t -> begin
      if n = 0 then (h, not_picked @ t)
      else extract (h :: not_picked) (n - 1) t
     end  *)
  let picked = List.nth lst n in
  let not_picked =  List.filter 
      (fun elem -> elem <> picked) lst in
  (picked, not_picked)

(** [extract_n lst n acc op] extracts n elements and put then in a new list and
    get the list with remaining elements after extraction. It either extract 
    randomly or sequentially depending on [op].
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

(** [shuffle_list lst] is the shuffled [lst]. *)
let shuffle_list lst = 
  let shuffled, left = extract_n lst (List.length lst) [] extract_rand in
  shuffled

(** [extract_seq lst] extracts the first element in [lst]. It is the op passed 
    into [extract_n] for sequentially extracting the first n elements. *)
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

(** [after_chii state current_player last_discarded] is the state of game after
    [current_player] at [state] perform action chii. It calls pre-defined 
    functions to decide if the player is qualified to chii, calculates all 
    combination the player can build from chii, and update game state.  *)
let rec after_chii current_player last_discarded hand_dark state =
  let all_chii_combo = Tile.all_pos hand_dark last_discarded in
  print_endline {|You can chii the last discarded tile.|};
  print_endline (string_of_all_combos all_chii_combo);
  print_endline {|Which combo you would like to Chii? E.g. Chii 1\n>>|};
  (* if Tile.chii_legal hand_dark last_discarded *)
  chii_helper state last_discarded all_chii_combo current_player hand_dark

(** [chii_helper state last hand_dark current_player] displays all possible 
    combinations the player can build by chii and update their hand according
    to the combo they command to build. *)
and chii_helper state last combos current_player hand_dark = 
  match Command.parse (read_line ()) with
  | Chii n when 0 < n && n <= List.length combos -> 
    Player.chii_update_handtile (n - 1) last current_player; state
  | Chii n -> 
    print_endline "Please choose from the given option\n>>";
    after_chii current_player last hand_dark state 
  | Discard (kind, number) -> 
    print_endline "You can't discard now."; 
    after_chii current_player last hand_dark state 
  | Ron -> 
    print_endline "You can't Ron now. Good luck with the rest of the game!"; 
    after_chii current_player last hand_dark state 
  | Quit -> 
    print_endline "Thank you for playing, bye!";
    {state with in_game = false}

(** [string_of_all_combos combos] convert a list of combinations of tiles to 
    string for easy-printing. *)
and string_of_all_combos combos = 
  let order = ref 0 in
  List.fold_left (fun acc combo -> 
      order := !order + 1; 
      acc ^ (string_of_int !order) ^ (string_of_combo combo)) "" combos

(** [string_of_combo combo] convert a combination of tiles represented by a 
    list [combo] to a string. *)
and string_of_combo combo =
  List.fold_left 
    (fun acc tile -> acc ^ "  " ^ Tile.string_of_tile tile) "" combo

(** [after_draw state current_player] is the state of game after
    [current_player] at [state] draws a card. It takes the first tile from 
    the randomized pile and put it in player's hand. *)
let after_draw current_player state =
  let drawn_tile = List.hd state.wall_tiles in
  Player.draw_tile current_player drawn_tile;
  print_endline {|Drawn tile:\n|};
  print_endline (Tile.string_of_tile drawn_tile);
  state

(** [after_discard state current_player] is the state of game after
    [current_player] at [state] discard a tile. *)
let rec after_discard current_player state =
  print_endline {|Enter command to discard a tile. E.g. "discard Man 1"\n>>|};
  match Command.parse (read_line ()) with
  | Discard (kind, number) -> discard_helper current_player kind number state
  | Chii n -> print_endline "You can't Chii now."; state
  | Ron -> 
    print_endline "You can't Ron now. Good luck with the rest of the game!"; 
    state
  | Quit -> print_endline "Quitting!"; { state with in_game = false }

(** [discars_helper] discards the tile <[kind] [number]> commanded by [
    current_player] if the tile is found in their dark hand, and prompt them 
    to re-enter command otherwise. *)
and discard_helper current_player kind number state = 
  let tile_opt = 
    Tile.find_tile kind number (Player.hand_tile_dark current_player) in
  match Player.discard_tile current_player tile_opt with
  | true -> state
  | false -> 
    print_endline "You don't have this tile.";
    after_discard current_player state

(** [riichi_helper state current_player to_get] changes the state for 
    [current_player] to richii and display what tiles are needed for them to 
    win. *)    
let riichi_helper current_player to_get state =
  print_endline (string_of_combo to_get);
  Player.riichi current_player;
  state

(** [after_check_richii state current_player] is the game state after we check
    if [current_player] can richii according to the rule. It calls 
    [riichi_helper] to perform the richii action display more info for the 
    player. *)
let after_check_richii current_player state =
  let to_get = Player.check_riichi current_player in
  if to_get = [] then state
  else riichi_helper current_player to_get state

(** [afte_check_rong this_plr last_plr tile state] is the state after we check
    if [this_plr] has won. Game ends if won. *)
let after_check_rong this_plr tile state =
  let hand = Player.hand_tile_dark this_plr 
             @ Player.hand_tile_light this_plr 
             @ [tile] in
  let riichi_state = Player.state_r this_plr in 
  let comb_for_rong = Player.ini_comb hand in (* TODO: add riichi state *)
  match Player.check_triplet comb_for_rong with
  | true -> print_endline "Rong! Congratulations, you win the game!";
    {state with in_game = false}
  | false -> state


(** [next_state state] is the game state after a player has played their turn.
    In a turn, a player will draw and discard tile, and if possible chii, 
    riichi, or win. *)
let rec next_state state = 
  let last_plr_id = 
    if state.current_player_id <= 1 then 4
    else state.current_player_id - 1 in
  let last_player = 
    state.players |>  extract [] (last_plr_id - 1) |> fst in
  let last_tile_opt =
    try Some (last_player |> Player.discard_pile |> extract [] 0 |> fst) with
    | Not_found -> None in
  let this_player = 
    state.players |> extract [] (state.current_player_id - 1) |> fst in
  let hand_dark = Player.hand_tile_dark this_player in
  match last_tile_opt with
  | Some last_tile when Tile.chii_legal hand_dark last_tile -> 
    chii_routine this_player last_tile hand_dark state 
  | Some last_tile -> 
    not_chii_routine this_player last_player last_tile state 
  | None -> first_round_routine this_player last_player state 
(* let after_chii_state = after_chii state this_player last_discarded_tile in
   let after_draw_state = after_draw after_chii_state this_player in
   let last_state = after_discard after_draw_state this_player in
   if List.length last_state.wall_tiles <= 4 then 
   {last_state with in_game = false}
   else last_state *)

(** [first_round_routine this_plr last_plr state] is the state after this turn 
    if [this_plr] is the first one in the whole game to make a move. This means
    that chii is not possible and we skip to draw. *)
and first_round_routine this_plr last_plr state =
  let state_after_draw = after_draw this_plr state in
  let drawn_tile = this_plr |> Player.hand_tile_dark |> List.hd in
  match after_check_rong this_plr drawn_tile state_after_draw with
  | state' when state'.in_game -> 
    state' 
    |> after_discard this_plr 
    |> after_check_richii this_plr
  | state' -> state'

(** [chii_routine this_plr last_tile hand_dark state] is the state after this 
    turn if [this_plr] can perform chii action. In this case, the player cannot
    draw and we skip drawing and checking for rong to discarding tile. *)
and chii_routine this_plr last_tile hand_dark state = 
  match after_check_rong this_plr last_tile state with
  | state' when state'.in_game ->
    state'
    |> after_chii this_plr last_tile hand_dark 
    |> after_discard this_plr 
    |> after_check_richii this_plr
  | state' -> state'

(** [not_chii_routine this_plr last_plr last_tile state] is the state after 
    this turn if [this_plr] can not chii. This means they will draw, be checked
    for rong, and be checked for riichi. *)
and not_chii_routine this_plr last_plr last_tile state =
  let state1 = match after_check_rong this_plr last_tile state with
    | state' when state'.in_game ->
      state'
      |> after_draw this_plr
    | state' -> state' in
  let drawn_tile = this_plr |> Player.hand_tile_dark |> List.hd in
  match after_check_rong this_plr drawn_tile state1 with
  | state' when state'.in_game ->
    state' 
    |> after_discard this_plr 
    |> after_check_richii this_plr
  | state' -> state'


and riichi_routine this_plr last_plr last_tile state =
  failwith ""

let is_in_game state = state.in_game

let rec display_all_player players =
  match players with
  | [] -> ()
  | h :: t -> 
    print_string "\n player \n";
    Player.display_I h;
    display_all_player t


let display_game state = 
  failwith ""
