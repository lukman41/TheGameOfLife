(* open Yojson.Basic.Util *)
open ANSITerminal

type spot =
  | Start
  | CollectPay of {
      id : int;
      prompt : string;
      value : int;
      next : spot option;
    }
  | LifeChoice of {
      id : int;
      prompt : string;
      next : spot option;
    }

type board = spot list

type career = {
  name : string;
  salary : int;
}

type player = {
  name : string;
  money : int;
  career : career option;
  position : spot;
}

type t = {
  current_player : player;
  players : player list;
  game_board : board;
}

let make_player name career = { name; career; money = 10000; position = Start }
let player_name player = player.name

let player_payday game salary =
  let new_players =
    let new_player =
      {
        name = game.current_player.name;
        career = game.current_player.career;
        money = game.current_player.money + salary;
        position = game.current_player.position;
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
  let r = Random.int 12 in
  if r = 0 then 1 else r - 1

let move g s = g

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
              if (r = 0 || r = 1) then 1 else r - 1
            in
            ANSITerminal.print_string [ ANSITerminal.green ]
              (h.name ^ " spun a " ^ string_of_int player_spin);
            print_newline ();
            prompt_players t ((h, player_spin) :: assoc_spun_lst)
        | _ -> prompt_players p assoc_spun_lst)
  in
  let current_player, players = prompt_players players [] in
  { current_player; players; game_board = [] }
