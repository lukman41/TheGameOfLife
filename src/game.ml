(* open Yojson.Basic.Util *)

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
  let max_spin =
    List.map (fun x -> snd x) assoc_lst
    |> List.fold_left (fun acc x -> max acc x) 0
  in
  let rec spin_winners lst =
    match lst with
    | [] -> []
    | h :: t -> if snd h = max_spin then h :: spin_winners t else spin_winners t
  in
  (List.hd (spin_winners assoc_lst), players)

let first_turn_spin players =
  let rec prompt_players p assoc_spun_lst =
    match p with
    | [] -> find_max players assoc_spun_lst
    | h :: t -> (
        print_endline (h.name ^ ", type 'spin' to spin");
        match read_line () with
        | "spin" -> prompt_players t ((h, spin) :: assoc_spun_lst)
        | _ -> prompt_players p assoc_spun_lst)
  in
  prompt_players players []
