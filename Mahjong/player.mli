(** The abstract type of values representing player. *)
type t

(** p_id is the identifier of different player.
    Different player should have different id*)
type id = int

(** [p_id t] gets the id of player [t]*)
val p_id : t -> id

(** [state_r t] gets riichi state of player [t]*)
val state_r : t -> bool

(** [state_c t] gets chii state of player [t]*)
val state_c : t -> bool

(** [hand_tile_light t] gets all the light hand tiles of the player [t]*)
val hand_tile_light : t -> Tile.t list

(** [hand_tile_dark r] gets all the dark hand tiles of the player [t]*)
val hand_tile_dark : t -> Tile.t list

(** [discard_pile t] gets the list of discarded tiles of the player [t]*)
val discard_pile : t -> Tile.t list

(** [draw_tile] puts tile into the player's dark tiles. *)
val draw_tile : t -> Tile.t -> unit

(** [discard_tile t tile_opt] play a tile with id in t, return false if the tile
   that player wants to discard is not in their hand tiles.  *)
val discard_tile: t -> Tile.t option -> bool

(** [check_riichi t] checks if the player [t] can riichi. 
  return an empty list if the player cannot riichi*)
val check_riichi: t -> Tile.t list

(**  [riichi t] changes the status from normal to riichi *)
val riichi: t -> unit

(** [display_I t] displays a player's handtile *)
val display_I : t -> unit

(**  [d_list tile_lst] displays the tiles of current player *)
val d_list: Tile.t list -> unit

(** [init_player id richii chii light dark discard] constructor for a player *)
val init_player: int -> bool -> bool -> Tile.t list -> Tile.t list -> 
  Tile.t list -> t

type comb = {
  (**  Players handtile in the form of (Tile, number_of_tiles) list. 
  For example: [Sou1; Sou2; Sou2] is stored as [(Sou1, 1); (Sou2, 2)]*)
  info: (Tile.t * int) list;

  (**  the pair in user's handtile*)
  pair: Tile.t list;
  
  (**  the triplets in user's handtile*)
  triplet: Tile.t list list;
    
  (**  the sequence in user's handtile*)
  seq: Tile.t list list;

  (**  indicate if user has riichied or not*)
  riichied: bool;
}

(** [ini_com tile_lst] is the constructor for comb *)
val ini_comb: Tile.t list -> bool -> comb

(** [ini_info lst acc] takes in a list [lst] of tiles and return a list [acc] 
   of tuples [(tile, count)] *)
val ini_info: Tile.t list -> (Tile.t * int) list -> (Tile.t * int) list

(**  [print_info info] prints info*)
val print_info: (Tile.t * int) list -> unit

(**  [check_triplet comb] checks if user is able to ron*)
val check_triplet : comb -> bool

(**  [chii_update_handtile int tile player] updates handtile and state_c of
[player]. [int] is the n-th option [player] chose among all options, [tile] is 
the tile [player] wants to chii *)
val chii_update_handtile : int -> Tile.t -> t -> unit

