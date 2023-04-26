(* open Yojson.Basic.Util *)
open ANSITerminal

type spot =
  | Start of { next : spot option }
  | Retire of { next : spot option }
  | Payday of { next : spot option }
  | Action of { next : spot option }
  | Married_Stop of {
      prompt : string;
      next : spot option;
    }
  | Family_Stop of {
      prompt : string;
      next : spot option;
    }
  | Crisis_Stop of {
      prompt : string;
      next : spot option;
    }
  | Retire_Early_Stop of {
      prompt : string;
      next : spot option;
    }
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
}

type t = {
  current_player : player;
  players : player list;
  game_board : board;
}

let make_player name career =
  {
    name;
    career;
    money = 250000;
    position = Start { next = None };
    houses = [];
    pegs = 0;
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
      }
    in
    List.tl game.players @ [ new_player ]
  in
  {
    current_player = List.hd new_players;
    players = new_players;
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
  | Married_Stop { prompt; next } -> next
  | Family_Stop { prompt; next } -> next
  | Crisis_Stop { prompt; next } -> next
  | Retire_Early_Stop { prompt; next } -> next
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
  }

let rec move_helper g spin_number =
  match spin_number with
  | 0 -> (
      match g.current_player.position with
      | x (* every spot type *) -> g)
  | _ ->
      let moved_player = move_player_spot g.current_player in
      {
        current_player = moved_player;
        players = moved_player :: List.tl g.players;
        game_board = g.game_board;
      }
(* patten match on current players spot to check if its payday or stop or
   retire *)

let move g =
  let s = spin in
  move_helper g s

let rec move_helper g s =
  match s with
  | 0 -> g
  | x -> g

let end_game g =
  let lst_players = g.players in
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
  let current_player, players = prompt_players players [] in
  { current_player; players; game_board = [] }
