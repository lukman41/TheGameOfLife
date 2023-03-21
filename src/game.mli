type career
type player
type t

val spin : int
(** [spin] is a randomly generated number from [1, 10] inclusive*)

val move : t -> int -> t
(** [move g s] is the updated game after the current user moves [s] spots. It
    alters the current player and the player list *)

val end_game : t -> string
(** [end_game g] is the name of the winner of the game based on their money
    amounts *)

val make_player : string -> career option -> player
(* [make_player n c] takes in a name and a career and returns a player *)

(* val player_name : player -> string
[player_name p ] take in a player and returns the name field of the player *)

(* val first_turn_spin : player list -> player * player list
(* [first_turn_spin l p] takes in a player list and after spinning for all
   players returns the player which will go first as well as the list of players
   in order of their turn *) *)
