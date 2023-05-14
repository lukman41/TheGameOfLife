open OUnit2
open Game_of_life
open Game
open Command
open Cards
open Yojson.Basic.Util

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let spin_test (name : string) (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output spin

let spin_test (name : string) (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output spin

let set_player_money_test (name : string) (input : bool) (expected_output : int)
    : test =
  name >:: fun _ -> assert_equal expected_output (Game.set_player_money input)

let set_player_money_tests =
  [
    set_player_money_test "going to college is true" true 150000;
    set_player_money_test "going to college is false" false 250000;
  ]

let game_tests = [ set_player_money_tests ]
let suite = "test suite for Game of Life" >::: List.flatten game_tests
let _ = run_test_tt_main suite
