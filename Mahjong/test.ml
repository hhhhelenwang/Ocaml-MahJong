open OUnit2
open Player 
open Tile
open Game

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"


(* Start of tests **********************************)

(* Player tests ******************)
let discard_tile_test
    (name : string)
    (player : Player.t)
    (tid : Tile.id)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (discard_tile player tid))

let tile1 = Tile.construct 1 Man 1 true
let tile2 = Tile.construct 2 Sou 3 false
let tile3 = Tile.construct 4 Man 2 true
let tile4 = Tile.construct 5 Man 7 false
let tile5 = Tile.construct 6 Wind 2 false
let tile6 = Tile.construct 7 Dragon 2 false
let tile7 = Tile.construct 8 Dragon 1 false

let t_list1 = [tile2; tile4]
let t_list2 = [tile1; tile2; tile1; tile2; tile4; tile5; tile6; tile7]
let dark1 = [tile1; tile2; tile3; tile4; tile5; tile6; tile7]

let player1 = Player.init_player 1 false false [] dark1 t_list1

(* let sorted_tiles = Player.d_list (Tile.sort t_list2) *)

let player_tests = 
  [
    discard_tile_test "discard one existing tile" player1 2 true;
    discard_tile_test "discard one not existed tile" player1 3 false;
  ]
let player_handt = Player.display_I player1


let x = Player.d_list t_list1

(* Game tests ******************)
let init_deck = init_state ()
let game1 = Game.make_game init_deck
let game2 = Game.make_game init_deck

let print_result1 = display_game game1
let print_result2 = display_game game2

let suite =
  "test suite for Mahjong"  >::: List.flatten [
    player_tests;
  ]

let _ = run_test_tt_main suite
