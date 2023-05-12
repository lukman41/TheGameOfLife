open Game
open Yojson.Basic.Util

(**Takes in the spot from the json and returns the spot's type*)
let get_spot_type json = json |> member "name" |> to_string

(** Takes in the board json and outputs a list with the spot_types as strings*)
let board_from_json json =
  json |> member "board" |> to_list |> List.map get_spot_type

let rec make_board_helper acc prev string_board =
  let backward_board = List.rev string_board in
  match backward_board with
  | [] -> acc
  | h1 :: h2 :: t ->
      if h1 = "Retire" then
        make_board_helper (retire_spot :: acc) retire_spot (h2 :: t)
      else
        let new_spot = make_spot h1 prev in
        make_board_helper (new_spot :: acc) new_spot (h2 :: t)
  | h :: t ->
      let new_spot = make_spot h prev in
      new_spot :: acc

let make_board board = make_board_helper [] retire_spot board
let start_spot board = List.hd board
