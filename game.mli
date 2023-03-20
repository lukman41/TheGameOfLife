(*my name is BLeu*)
type t
(* the current game *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the board that [j] represents. Requires: [j] is a valid
    JSON board representation. *)

val get_state : t -> t
