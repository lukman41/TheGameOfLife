open OUnit2
open Game_of_life
open Game
open Command
open Cards
open Board

let spin_test (name : string) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (spin)

let spin_test (name : string) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (spin)

let game_tests = [
  spin_test;
  ]

let suite = "test suite for Game of Life" >::: List.flatten []
let _ = run_test_tt_main suite
