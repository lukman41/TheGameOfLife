open Game
open Yojson.Basic.Util

(**Takes in a single spot from the json and returns the type of spot as a string*)
let get_spot_type json = json |> member "name" |> to_string

(** Takes in the board json and outputs it as a list in order with the
    spot_types as strings*)
let board_from_json json =
  json |> member "board" |> to_list |> List.map get_spot_type

(**Builds an accumulator for the board as a spot list. Keeps track of the
   previous spot we just worked with in order to have next when making the
   current spot*)
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

(**Makes the board with an empty accumulator *)
let make_board board = make_board_helper [] retire_spot board

(**Returns the start of the board *)
let start_spot board = List.hd board
