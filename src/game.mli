type career
type player
type t
type board

val spin : int
(** [spin] is a randomly generated number from [1, 10] inclusive*)

val move : t -> t
(** [move g s] is the updated game after the current user moves [s] spots. It
    alters the current player and the player list *)

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
