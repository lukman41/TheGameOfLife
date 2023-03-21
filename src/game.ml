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
  career : career;
  position : spot;
}

type t = {
  current_player : player;
  players : player list;
  game_board : board;
}

let make_player name career = { name; career; money = 10000; position = Start }

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
