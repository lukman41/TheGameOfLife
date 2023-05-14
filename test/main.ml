open OUnit2
open Game_of_life
open Game
open Command
open Cards
open Board

let suite = "test suite for Game of Life" >::: List.flatten []
let _ = run_test_tt_main suite
