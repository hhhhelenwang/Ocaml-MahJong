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


(** Rong test- *)
(* tile: id kind num isDiscarded *)
(* 111222333444 
   11 22 33 4567 89
*)
let t1 = Tile.construct 1 Man 1 false
let t2 = Tile.construct 1 Man 2 false
let t3 = Tile.construct 1 Man 3 false
let t4 = Tile.construct 1 Man 4 false
let t5 = Tile.construct 1 Man 5 false
let t6 = Tile.construct 1 Man 6 false
let t7 = Tile.construct 1 Man 7 false
let t8 = Tile.construct 1 Man 8 false
let t9 = Tile.construct 1 Man 9 false

let t11 = Tile.construct 1 Sou 1 false
let t12 = Tile.construct 1 Sou 2 false
let t13 = Tile.construct 1 Sou 3 false
let t14 = Tile.construct 1 Sou 4 false
let t15 = Tile.construct 1 Sou 5 false
let t16 = Tile.construct 1 Sou 6 false
let t17 = Tile.construct 1 Sou 7 false
let t18 = Tile.construct 1 Sou 8 false
let t19 = Tile.construct 1 Sou 9 false

let ron_l1= [t1;t1;t1; t2;t2;t2; t3;t3;t3; t4;t4;t4; t5;t5]
let ron_l2= [t1;t2;t3; t7;t8;t9; t11;t12;t13; t17;t18;t19; t5;t5]


let ron_l3 = [t1;t1;t2;t2; t3;t3;t4;t5; t6;t7;t8;t8; t11; t11]

let n_comb1 = Player.ini_comb ron_l1
let n_comb2 = Player.ini_comb ron_l2
let n_comb3 = Player.ini_comb ron_l3 

let ron_test
    (name : string)
    (com : Player.n_comb)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Player.di_gui com))

let ron_tests = [
  ron_test "111 222 333 444 55" n_comb1 true;
  ron_test "123 789 123 789 55" n_comb2 true;



  ron_test "1122334567 88 " n_comb3 false;
]



let suite =
  "test suite for Mahjong"  >::: List.flatten [
    player_tests;
    ron_tests;
  ]

let _ = run_test_tt_main suite
