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

<<<<<<< HEAD
val make_player : string -> career option -> player
(* [make_player n c] takes in a name and a career and returns a player *)
=======
val make_player : string -> string -> player
(* [make_player n c] takes in a name and a career an returns a player at the
   start of the game *)
>>>>>>> 753e3e6eca8ccfd5235cd9e5ac7d841686b02f50
