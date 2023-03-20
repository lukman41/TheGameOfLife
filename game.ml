(* open Yojson.Basic.Util *)

type spot =
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
  money : int;
  career : career;
  position : spot;
}

type t = {
  current_player : player;
  players : player list;
  game_board : board;
}
