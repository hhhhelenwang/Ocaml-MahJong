(** 
   Representation of dynamic game state.
   This module represents the state of a game as it is being played. This 
   includes the current wall tiles and current state of the player.
*)

(** The abstract type of values representing the game state. *)
type t

(** [init_state] is the game state before the game starts. No tile is given to
    players. Number of tiles and players should be provided *) 
val init_state : int -> int -> t

(** [make_game tiles] is the initial state of the game. It randomly gives
    each player 13 tiles. *)
val make_game : t -> t

(** [next_state] is the new state of the game after one player has made a 
    move. *)
val next_state : t -> t