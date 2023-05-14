open OUnit2
open Game_of_life
open Game
open Command
open Cards
open Yojson.Basic.Util

let career1 : career =
  {
    name = "Software Engineer";
    salary = 250000;
    bonus_salary = 100000;
    requires_degree = false;
  }

let actionspot : spot = Action { next = None }

let player1 =
  {
    name = "David";
    money = 150000;
    career = Some career1;
    position = actionspot;
    houses = [];
    pegs = 1;
    has_degree = true;
  }

let darielis =
  {
    name = "Darielis";
    money = 150000;
    career = Some career1;
    position = actionspot;
    houses = [];
    pegs = 1;
    has_degree = true;
  }

let game1 =
  {
    current_player = player1;
    active_players = [ player1 ];
    retired_players = [];
    game_board = [];
  }

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

<<<<<<< Updated upstream
let spin_test (name : string) (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output spin
=======
let spin_test (name : string) (expected_output_min : int)
    (expected_output_max : int) : test =
  name >:: fun _ ->
  assert_equal true (expected_output_min <= spin && expected_output_max >= spin)
>>>>>>> Stashed changes

let set_player_money_test (name : string) (input : bool) (expected_output : int)
    : test =
  name >:: fun _ -> assert_equal expected_output (Game.set_player_money input)

let parse_test (name : string) (input : string) (expected_output : command) :
    test =
  name >:: fun _ -> assert_equal expected_output (Command.parse input)

let parse_test_empty_exn (name : string) (input : string) : test =
  name >:: fun _ -> assert_raises Empty (fun () -> Command.parse input)

let parse_test_malformed_exn (name : string) (input : string) : test =
  name >:: fun _ -> assert_raises Malformed (fun () -> Command.parse input)

let player_name_test (name : string) (input : player) (expected_output : string)
    : test =
  name >:: fun _ -> assert_equal expected_output (Game.player_name input)

let set_player_money_tests =
  [
    set_player_money_test "going to college is true" true 150000;
    set_player_money_test "going to college is false" false 250000;
  ]

let parsing_test =
  [
    parse_test "parsing spin" "spin" Spin;
    parse_test "parsing quit" "quit" Quit;
    parse_test "parsing draw" "draw" Draw;
    parse_test "parsing start" "start" Start;
<<<<<<< Updated upstream
    parse_test "parsing choose" "choose yes" (Choose [ "yes" ]);
    parse_test "parsing choose" "choose no" (Choose [ "no" ]);
    parse_test "parsing choose with spaces" " choose no" (Choose [ "no" ]);
    parse_test "parsing choose with spaces on yes" " choose             yes" (Choose [ "yes" ]);
    parse_test "parsing change" "change house" (Change [ "house" ]);
    parse_test "parsing change" "change career" (Change [ "career" ]);
    parse_test_empty_exn "empty string" "";
    parse_test_malformed_exn "typo" "chnge career";
  ]

let player_name_tests =
  [
    player_name_test "test for david" player1 "David";
    player_name_test "test for Darielis" darielis "Darielis";
=======
    spin_test "spin test 1" 1 4;
    spin_test "spin test 2" 1 4;
    spin_test "spin test 3" 1 4;
    spin_test "spin test 4" 1 4;
    spin_test "spin test 5" 1 4;
    spin_test "spin test 6" 1 4;
    spin_test "spin test 7" 1 4;
    spin_test "spin test 8" 1 4;
    spin_test "spin test 9" 1 4;
    spin_test "spin test 10" 1 4;
    spin_test "spin test 11" 1 4;
    spin_test "spin test 12" 1 4;
>>>>>>> Stashed changes
  ]

let game_tests = [ set_player_money_tests; parsing_test ]
let suite = "test suite for Game of Life" >::: List.flatten game_tests
let _ = run_test_tt_main suite
