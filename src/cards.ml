open Game
open Yojson.Basic.Util

type action_cards =
  | Pay of {
      id : string;
      name : string;
      prompt : string;
      amount : int;
    }
  | Spin of {
      id : string;
      name : string;
      prompt : string;
      even_amount : int;
      odd_amount : int;
    }
  | Choice of {
      id : string;
      name : string;
      prompt : string;
      a_amount : int;
      b_amount : int;
    }

type house_card = {
  id : string;
  name : string;
  price : int;
  even_amount : int;
  odd_amount : int;
}

type career_card = {
  id : string;
  name : string;
  salary : int;
  bonus : int;
}
