type spot =
  | Start of { next : spot option }
  | Retire of { next : spot option }
  | Payday of { next : spot option }
  | Action of { next : spot option }
  | MarriedStop of {
      next : spot option;
          (*two options on which path to take, when a path is chosen, you could
            set players current spot to married_spot with the next being one of
            the choices from the tuple*)
    }
  | FamilyStop of { next : spot option }
  | CrisisStop of { next : spot option }
  | GraduationStop of { next : spot option }
  | House of { next : spot option }
  | Friend of { next : spot option }
  | Pet of { next : spot option }
  | Baby of { next : spot option }
  | Twins of { next : spot option }
  | Career of { next : spot option }

type board = spot list

type career = {
  name : string;
  salary : int;
  bonus_salary : int;
  requires_degree : bool;
}

type house = {
  name : string;
  purchase_price : int;
  sell_even : int;
  sell_odd : int;
}

type player = {
  name : string;
  money : int;
  career : career option;
  position : spot;
  houses : house list;
  pegs : int;
  has_degree : bool;
}

type t = {
  current_player : player;
  active_players : player list;
  retired_players : player list;
  game_board : board;
}

val set_player_career : bool -> string -> career option
(** [set_player_career b] returns whether the player's starting career will be
    based on if they chose to go college (None) or not (a career that dosent
    require a degree)*)

val spin : int
(** [spin] is a randomly generated number from [1, 10] inclusive*)

val move_current_player : t -> int -> t
(** [move_current_player g i] returns the updated game state after one players
    full turn in the game, encompassing their spin, moving across the board and
    any actions completed. this function handles switching of current players
    after one players turn.s*)

val end_game : t -> string
(** [end_game g] is the name of the winner of the game based on their money
    amounts *)

val make_player : string -> bool -> player
(* [make_player n c] takes in a name and a choice of college or not and returns
   a player *)

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

val landed_spot_operations : t -> t
(**[landed_spot_operations g] preforms the actions needed for the spot that a
   player lands on.*)

val passed_spot_operations : t -> int -> t
(**[passed_spot_operations g] preforms the actions needed for the spot that a
   player passes.*)

val active_players : t -> player list
(* [active_players g] returns a list of the active players in the game state *)

val current_player : t -> player
(* [current_player g] returns the current player in game g *)

val current_player_name : player -> string
val retire_spot : spot

val make_spot : string -> spot -> spot
(** [make_spot s n] is the Spot object with type option s and next spot n *)

val prompt_for_spin : t -> int
(** [prompt_for_spin g] takes in a game g and prompts the current player to spin
    and returns the result*)

val set_player_money : bool -> int
(** [set_player_money c] takes in a choice and returns the amount of money the
    player will start with*)

val add_pegs : t -> int -> t
(**[add_pegs g a] takes in a game and returns a new game state with the
   current_player having i more pegs added *)

val find_max : 'a -> ('b * 'c )list -> 'b * 'b list
(**[find_max p a] takes in [a], an association list for the players and the
   number spun, and returns the updated order based on who gets the highest spin*)
