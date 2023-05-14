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
let before_action = Payday { next = Some actionspot }

let player1 =
  {
    name = "Miah";
    money = 150000;
    career = Some career1;
    position = actionspot;
    houses = [];
    pegs = 1;
    has_degree = true;
  }

let add1player1 =
  {
    name = "Miah";
    money = 150000;
    career = Some career1;
    position = actionspot;
    houses = [];
    pegs = 2;
    has_degree = true;
  }

let darielis =
  {
    name = "Darielis";
    money = 150000;
    career = Some career1;
    position = actionspot;
    houses = [];
    pegs = 2;
    has_degree = true;
  }

let add2player1 =
  {
    name = "Miah";
    money = 150000;
    career = Some career1;
    position = actionspot;
    houses = [];
    pegs = 3;
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

let spin_test (name : string) (expected_output_min : int)
    (expected_output_max : int) : test =
  name >:: fun _ ->
  assert_equal true (expected_output_min <= spin && expected_output_max >= spin)

let set_player_money_test (name : string) (input : bool) (expected_output : int)
    : test =
  name >:: fun _ -> assert_equal expected_output (Game.set_player_money input)

let parse_test (name : string) (input : string) (expected_output : command) :
    test =
  name >:: fun _ -> assert_equal expected_output (Command.parse input)

let active_players_test (name : string) input (expected_output : player list) :
    test =
  name >:: fun _ -> assert_equal expected_output (Game.active_players input)

let current_player_test (name : string) input (expected_output : player) : test
    =
  name >:: fun _ -> assert_equal expected_output (Game.current_player input)

let current_player_name_test (name : string) input (expected_output : string) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Game.current_player_name input)

let add_peg_test (name : string) input amount expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Game.add_pegs input amount)

let parse_test_empty_exn (name : string) (input : string) : test =
  name >:: fun _ -> assert_raises Empty (fun () -> Command.parse input)

let parse_test_malformed_exn (name : string) (input : string) : test =
  name >:: fun _ -> assert_raises Malformed (fun () -> Command.parse input)

let player_name_test (name : string) (input : player) (expected_output : string)
    : test =
  name >:: fun _ -> assert_equal expected_output (Game.player_name input)

let get_next_position_test (name : string) input expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Game.get_next_position input)

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
    parse_test "parsing choose" "choose yes" (Choose [ "yes" ]);
    parse_test "parsing choose" "choose no" (Choose [ "no" ]);
    parse_test "parsing choose with spaces" " choose no" (Choose [ "no" ]);
    parse_test "parsing choose with spaces on yes" " choose             yes"
      (Choose [ "yes" ]);
    parse_test_empty_exn "parsing choose with empty string" "choose";
    parse_test_malformed_exn "parsing choose with malformed string" "chse";
    parse_test_empty_exn "parsing change with empty string" "change";
    parse_test_malformed_exn "parsing change with malformed string" "chnge";
    parse_test "parsing change" "change house" (Change [ "house" ]);
    parse_test "parsing change" "change career" (Change [ "career" ]);
    parse_test_empty_exn "empty string" "";
    parse_test_malformed_exn "typo" "chnge career";
  ]

let player_name_tests =
  [
    player_name_test "test for Miah" player1 "Miah";
    player_name_test "test for Darielis" darielis "Darielis";
    player_name_test "test for Miah in add1player1" add1player1 "Miah";
    player_name_test "test for Miah in add2player1" add2player1 "Miah";
  ]

let spin_tests =
  [
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
  ]

let active_player_tests =
  [ active_players_test "Check active players list" game1 [ player1 ] ]

let current_player_tests =
  [ current_player_test "Check current player" game1 player1 ]

let add_peg_tests =
  [
    add_peg_test "Add no pegs" game1 0
      {
        current_player = player1;
        active_players = [ player1 ];
        retired_players = [];
        game_board = [];
      };
    add_peg_test "Add 1 peg" game1 1
      {
        current_player = add1player1;
        active_players = [ add1player1 ];
        retired_players = [];
        game_board = [];
      };
    add_peg_test "Add 2 pegs" game1 2
      {
        current_player = add2player1;
        active_players = [ add2player1 ];
        retired_players = [];
        game_board = [];
      };
  ]

let get_next_position_tests =
  [
    get_next_position_test "next is None" actionspot None;
    get_next_position_test "next is Action" before_action (Some actionspot);
  ]

let game_tests =
  [
    set_player_money_tests;
    parsing_test;
    player_name_tests;
    spin_tests;
    active_player_tests;
    current_player_tests;
    add_peg_tests;
    get_next_position_tests;
  ]

let suite = "test suite for Game of Life" >::: List.flatten game_tests
let _ = run_test_tt_main suite
