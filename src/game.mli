type career
type player
type t
type board
type spot

val spin : int
(** [spin] is a randomly generated number from [1, 10] inclusive*)

val move_current_player : t -> t
(** [move_current_player g] returns the updated game state after one players
    full turn in the game, encompassing their spin, moving across the board and
    any actions completed *)

val end_game : t -> string
(** [end_game g] is the name of the winner of the game based on their money
    amounts *)

val make_player : string -> career option -> player
(* [make_player n c] takes in a name and a career and returns a player *)

val player_name : player -> string
(* [player_name p ] take in a player and returns the name field of the player *)

val player_payday : t -> int -> t
(*[player_payday g s] takes the current game_state where the current_player
  needs a payday, it gives the the payday and updates the game_state to have the
  next player in the list become the current player *)

val first_turn_spin : player list -> t
(** [first_turn_spin l p] takes in a player list and after spinning for all
    players returns the updated game state *)

val get_next_position : spot -> spot option
(** [get_next_position s] takes in a spot and returns the next spot after the
    spot that was passed in, either none or Some spot*)

val move_player_spot : player -> player
(** [move_player_spot p] moves input players spot up by one*)

val prompt_for_spin : player -> int
(**[prompt_for_spin p] prompts a player through the command shell to spin, and
   returns the int of their spin number*)
