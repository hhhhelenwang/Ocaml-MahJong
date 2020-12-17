open OUnit2
open Player 
open Tile
open Game
open Command

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
    (tid : Tile.t option)
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
    discard_tile_test "discard one existing tile" player1 (Some tile2) true;
    discard_tile_test "discard one not existed tile" player1 None false;
  ]
(* let player_handt = Player.display_I player1 *)
(* let x = Player.d_list t_list1 *)


(* Game tests ******************)
let init_deck = init_state ()
let game1 = Game.make_game init_deck
let game2 = Game.make_game init_deck

(* let print_result1 = display_game game1
   let print_result2 = display_game game2 *)

(* Command Compilation Unit Tests *****************)
(** [command_parse_test] asserts that the parsed command is equal to the 
    ecpected command. *)
let command_parse_test
    (name : string)
    (str : string)
    (expected : command) : test = 
  name >:: (fun _ ->
      assert_equal expected (parse str))

(** [command_parse_test_exn] asserts that the expected exception is raised 
    when parsing an abnormal command. *)
let command_parse_text_exn
    (name : string)
    (str : string)
    (expected_exn) : test =
  name >:: (fun _ ->
      assert_raises expected_exn (fun () -> parse str))

let command_tests = [
  (* typical cases *)
  command_parse_test {|"discard Man 1" -> Discard (Man, 1)|}
    "discard Man 1" (Discard (Man, 1));
  command_parse_test {|"discard man 1" with spaces -> Discard (Man, 1)|}
    "      discard       Man 1" (Discard (Man, 1));
  command_parse_test {|"discard Dragon 1" -> Discard (Man, 1)|}
    "discard Dragon 1" (Discard (Dragon, 1));
  command_parse_test {|"chii 1" -> Chii 1|} "chii 1" (Chii 1);
  command_parse_test {|"Quit" -> Quit|} "quit" Quit;

  (* exn raised *)
  command_parse_text_exn {|"whatever Man 1" -> Malformed|} 
    "whatever Man 1" Malformed;
  command_parse_text_exn {|"Discard Man" -> Malformed|} 
    "Discard Man" Malformed;
  command_parse_text_exn {|"Discard something" -> Malformed|} 
    "Discard something" Malformed;
  command_parse_text_exn {|"" -> Empty|} "" Empty;
]
(** Rong test- *)
(* tile: id kind num isDiscarded *)
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

let t21 = Tile.construct 1 Dragon 1 false
let t22 = Tile.construct 1 Dragon 2 false
let t23 = Tile.construct 1 Dragon 3 false

let ron_l1= [t1;t1;t1; t2;t2;t2; t3;t3;t3; t4;t4;t4; t5;t5]
let ron_l2= [t1;t2;t3; t7;t8;t9; t11;t12;t13; t17;t18;t19; t5;t5]
let ron_l3 = [t1;t1;t2;t2; t3;t3;t4;t5; t6;t7;t8;t8; t11; t11]
let ron_l4 = [t1;t1;t1; t2;t2;   t3;t3;t3; t4;t4;t4; t5;t5;t5]
let ron_l5 = [t1;t2; t3;t3;t3;t3; t4;t4;t4;t4; t5;t5;t5;t5]
let ron_l6 = [t21;t21;t21; t22;t22;t22; t15;t16;t16;t16;t17; t23;t23;t23]
let ron_l7 = [t8;t9; t3;t3;t3;t3; t4;t4;t4;t4; t5;t5;t5;t5]
let ron_l8 = [t1;t2;t2;t2;t3;t4]
let ron_l9 = [ t2;t2; t5;t6;t7;t7;t8;t9;]
let ron_l10 = [t1;t1; t3;t3; t5;t5; t7;t7; t8;t8; t9;t9; t11;t11]

let n_comb1 = Player.ini_comb ron_l1
let n_comb2 = Player.ini_comb ron_l2
let n_comb3 = Player.ini_comb ron_l3 
let n_comb4 = Player.ini_comb ron_l4
let n_comb5 = Player.ini_comb ron_l5
let n_comb6 = Player.ini_comb ron_l6
let n_comb7 = Player.ini_comb ron_l7
let n_comb8 = Player.ini_comb ron_l8
let n_comb9 = Player.ini_comb ron_l9
let n_comb10 = Player.ini_comb ron_l10

let print_a_info= Player.print_info (Player.ini_info ron_l1 [])

let ron_test
    (name : string)
    (com : Player.comb)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Player.check_triplet com) 
        ~printer: string_of_bool)

let ron_tests = [
  ron_test "111 222 333 444 55" n_comb1 true;
  ron_test "123 789 123 789 55" n_comb2 true;
  ron_test "1122334567 88 11" n_comb3 false;
  ron_test "111 22 333 444 555 " n_comb4 true;
  ron_test "333 444 555 345 12" n_comb5 true;
  ron_test "rrr ggg 56667 www" n_comb6 true;
  ron_test "333 444 555 345 89" n_comb7 false;
  ron_test "cannot exhaust 12 234" n_comb8 false;
  ron_test "<14 tiles 22 567789" n_comb9 false;
  ron_test "7 pairs" n_comb10 true;
]

(* Tile tests ******************)

let rec display_ll lst = 
  let _ = print_string "[ "  in
  match lst with 
  | [] -> print_string "\n"
  | h :: t -> 
    let _= Player.d_list h in
    print_string " ]";
    display_ll t

let rec lst_to_string r =
  match r with
  | [] -> ""
  | h :: [] -> Tile.string_tile h
  | h :: t -> Tile.string_tile h ^ ";" ^ (lst_to_string t)

let rec pp_matrix matrix =
  match matrix with
  | [] -> ""
  | h :: t -> "[" ^ (lst_to_string h) ^ "];\n" ^ (pp_matrix t)

let all_pos_test
    (name : string)
    (lst : Tile.t list)
    (t : Tile.t)
    (expected_output : Tile.t list list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (all_pos lst t)
        ~printer:(pp_matrix))

let chii_legal_test
    (name : string)
    (lst : Tile.t list)
    (t : Tile.t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (chii_legal lst t))

let pos_l1 = [t1;t2;t3;t4;t5]
let pos_l2 = [t1;t2;t2;t2;t3;t4;t5]


let tile_tests = [
  (* all_pos_test "Man 12345, Man3" 
     pos_l1 t3 [[t1;t2;t3];[t3;t4;t5];[t2;t3;t4]];
     all_pos_test "Man 1 222 345, Man2" 
     pos_l2 t2 [[t2;t2;t2];[t2;t3;t4];[t1;t2;t3]]; *)

  chii_legal_test "Man 12345, Man3" pos_l1 t3 true;
  chii_legal_test "Man 12345, Sou1, dif kind" pos_l1 t11 false;
  chii_legal_test "Man 12345, Man 8, wrong num" pos_l1 t8 false;

]

let print_the_pos= display_ll (all_pos pos_l1 t3)
let print1 = display_ll (all_pos pos_l2 t2)

let intx= List.length (all_pos pos_l1 t3)
let _ = print_int intx

let suite =
  "test suite for Mahjong" >::: List.flatten [
    player_tests;
    command_tests;
    ron_tests;
    tile_tests;
  ]

let _ = run_test_tt_main suite
