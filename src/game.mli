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

val make_player : string -> string -> player
(* [make_player n c] takes in a name and a career an returns a player at the
   start of the game *)
