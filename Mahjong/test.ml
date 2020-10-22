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

let discard_tile_test
    (name : string)
    (player : Player.t)
    (tid : Tile.id)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (discard_tile player tid))

(* let player1 = {id = 1; state_r = false; state_c = false;
                 hand_tile = {light = []; dark = [];}; discard_pile = []} *)
let (tile1 : Tile.t) = {id = 1; kind = Man; number = 1; discarded = false}

(* let player_tests = 
  [
    discard_tile_test "discard one tile" player1 tid1 true;
  ] *)

let (tile2 : Tile.t) = {
  id = 2; kind = Sou; number = 3; discarded = false}
let t_list1 = [tile1; tile2]

let x = Player.d_list t_list1







let suite =
  "test suite for Mahjong"  >::: List.flatten [
    player_tests;
  ]

let _ = run_test_tt_main suite
