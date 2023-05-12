(* open Yojson.Basic.Util *)
open ANSITerminal

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
  | RetireEarlyStop of { next : spot option }
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
  requires_degree : bool;
}

type house = {
  name : string;
  purchase_price : int;
  sell_red : int;
  sell_black : int;
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

let set_player_career = function
  | true -> None
  | false -> failwith "function to draw career cards"

let make_player name choice pos =
  {
    name;
    career = set_player_career choice;
    money = 250000;
    position = pos;
    houses = [];
    pegs = 0;
    has_degree = false;
  }

let player_name player = player.name

let player_payday game salary =
  let new_players =
    let new_player =
      {
        name = game.current_player.name;
        career = game.current_player.career;
        money = game.current_player.money + salary;
        position = game.current_player.position;
        houses = [];
        pegs = game.current_player.pegs;
        has_degree = game.current_player.has_degree;
      }
    in
    List.tl game.active_players @ [ new_player ]
  in
  {
    current_player = List.hd new_players;
    active_players = new_players;
    retired_players = game.retired_players;
    game_board = game.game_board;
  }

let spin =
  Random.self_init ();
  let r = Random.int 10 in
  r + 1

let get_next_position pos =
  match pos with
  | Start { next } -> next
  | Retire { next } -> failwith "tried to move from retire spot"
  | Payday { next } -> next
  | Action { next } -> next
  | MarriedStop { next } -> next
  | FamilyStop { next } -> next
  | CrisisStop { next } -> next
  | RetireEarlyStop { next } -> next
  | GraduationStop { next } -> next
  | House { next } -> next
  | Friend { next } -> next
  | Pet { next } -> next
  | Baby { next } -> next
  | Twins { next } -> next
  | Career { next } -> next

let move_player_spot p =
  {
    name = p.name;
    money = p.money;
    career = p.career;
    position = Option.get (get_next_position p.position);
    houses = p.houses;
    pegs = p.pegs;
    has_degree = p.has_degree;
  }
let retired_list game = let new_list_players = List.filter 
(fun x -> x <> game.current_player) game.active_players in
let new_relist_players = [ game.current_player ] in
{
  current_player = List.hd new_list_players;
  active_players = new_list_players;
  retired_players = game.retired_players @ new_relist_players;
  game_board = game.game_board;
}
let landed_spot_operations g =
  (*all functions should return updated game.t*)
  match g.current_player.position with
  | Start _ -> failwith "unimplemented"
  | Retire _ ->
      failwith "Undone"
      (*function to take player out of game so they can wait for other players
        to finish*)
  | Payday _ ->
      failwith "unimplemented" (*funtion to pay players their bonus salary*)
  | Action _ -> failwith "unimplemented" (*function to draw action card*)
  | MarriedStop { next } ->
      failwith "unimplemented" (*function to perform stop choice*)
  | FamilyStop { next } ->
      failwith "unimplemented" (*function to perform stop choice*)
  | CrisisStop { next } ->
      failwith "unimplemented" (*function to perform stop choice*)
  | RetireEarlyStop { next } ->
      failwith "unimplemented" (*function to perform stop choice*)
  | GraduationStop { next } ->
      failwith "unimplemented"
      (* function to perform stop choice and check if you graduated *)
  | House _ ->
      failwith "unimplemented"
      (*function to draw a house card ask if player wants to buy, and *)
  | Friend _ -> failwith "unimplemented" (*function to perform add peg choice*)
  | Pet _ -> failwith "unimplemented" (*function to perform add peg choice*)
  | Baby _ -> failwith "unimplemented" (*function to perform add peg choice*)
  | Twins _ -> failwith "unimplemented" (*function to perform add peg choice*)
  | Career _ -> failwith "unimplemented" (*function to draw career card*)

let rec move_helper g spin_number =
  match spin_number with
  | 0 -> landed_spot_operations g
  | _ ->
      let moved_player = move_player_spot g.current_player in
      let game_with_player_moved =
        {
          current_player = moved_player;
          active_players = moved_player :: List.tl g.active_players;
          retired_players = g.retired_players;
          game_board = g.game_board;
        }
      in
      passed_spot_operations game_with_player_moved spin_number

and passed_spot_operations g spin_number =
  (* all branches sh ould call move helper again except retire. For example at a
     payday, pay out the bonus salary, then call move helper with one less spin.
     At a stop prompt and get the choice, then perform any necessary actions and
     prompt for player to spin again, calling move helper with that new spin
     number. We can use the same retire function whether you land on it or pass
     it. *)
  match g.current_player.position with
  | Retire _ ->
      failwith "Undone"
      (*function to take player out of game so they can wait for other players
        to finish*)
  | Payday _ ->
      failwith "unimplemented" (*function to pay players their bonus salary*)
  | MarriedStop { next } ->
      failwith "unimplemented" (*function to perform stop choice*)
  | FamilyStop { next } ->
      failwith "unimplemente" (*function to perform stop choice*)
  | CrisisStop { next } ->
      failwith "unimplemented" (*function to perform stop choice*)
  | RetireEarlyStop { next } ->
      failwith "unimplemented" (*function to perform stop choice*)
  | GraduationStop { next } ->
      failwith "unimplemented"
      (* function to perform stop and check if you graduated *)
  | _ -> move_helper g (spin_number - 1)
(*Since you dont do anything when you pass the rest of the spots,only when you
  land on them, we can just call the helper with the player moved one spot over
  and one less spot to go*)

let move_current_player g i = move_helper g i

let end_game g =
  let lst_players = g.active_players in
  let rec check_max lst max player =
    match lst with
    | [] -> player.name
    | h :: t ->
        if h.money > max then check_max t h.money h else check_max t max player
  in
  check_max lst_players 0 (List.hd lst_players)

let find_max players assoc_lst =
  let players =
    List.sort (fun x y -> max (snd x) (snd y)) assoc_lst
    |> List.map (fun x -> fst x)
  in
  (List.hd players, players)

let first_turn_spin players =
  print_endline "Take turns spinning the wheel. The highest spin will go first.";
  let rec prompt_players p assoc_spun_lst =
    match p with
    | [] -> find_max players assoc_spun_lst
    | h :: t -> (
        Stdlib.print_string (h.name ^ ", type ");
        ANSITerminal.print_string [ ANSITerminal.magenta ] "'s";
        ANSITerminal.print_string [ ANSITerminal.blue ] "p";
        ANSITerminal.print_string [ ANSITerminal.green ] "i";
        ANSITerminal.print_string [ ANSITerminal.yellow ] "n'";
        print_endline " to spin.";
        match read_line () with
        | "spin" ->
            let player_spin =
              Random.self_init ();
              let r = Random.int 12 in
              if r = 0 || r = 1 then 1 else r - 1
            in
            ANSITerminal.print_string [ ANSITerminal.green ]
              (h.name ^ " spun a " ^ string_of_int player_spin);
            print_newline ();
            prompt_players t ((h, player_spin) :: assoc_spun_lst)
        | _ -> prompt_players p assoc_spun_lst)
  in
  let current_player, active_players = prompt_players players [] in
  { current_player; active_players; retired_players = []; game_board = [] }

let active_players g = g.active_players
let current_player g = g.current_player
let current_player_name p = p.name
