open Game
open Yojson.Basic.Util

type action_cards =
  | Pay of {
      id : int;
      name : string;
      prompt : string;
      amount : int;
    }
  | Spin of {
      id : int;
      name : string;
      prompt : string;
      even_amount : int;
      odd_amount : int;
    }
  | Choice of {
      id : int;
      name : string;
      prompt : string;
      a_amount : int;
      b_amount : int;
    }

type house_card = {
  id : int;
  name : string;
  price : int;
  even_amount : int;
  odd_amount : int;
}

type career_card = {
  id : int;
  name : string;
  salary : int;
  bonus : int;
}

let action_pay_from_json json : action_cards =
  Pay
    {
      id = json |> member "id" |> to_int;
      name = json |> member "name" |> to_string;
      prompt = json |> member "prompt" |> to_string;
      amount = json |> member "amount" |> to_int;
    }

let action_spin_from_json json : action_cards =
  Spin
    {
      id = json |> member "id" |> to_int;
      name = json |> member "name" |> to_string;
      prompt = json |> member "prompt" |> to_string;
      even_amount = json |> member "even_amount" |> to_int;
      odd_amount = json |> member "odd_amount" |> to_int;
    }

let action_choice_from_json json : action_cards =
  Choice
    {
      id = json |> member "id" |> to_int;
      name = json |> member "name" |> to_string;
      prompt = json |> member "prompt" |> to_string;
      a_amount = json |> member "even_amount" |> to_int;
      b_amount = json |> member "odd_amount" |> to_int;
    }

let house_from_json json : house_card =
  {
    id = json |> member "id" |> to_int;
    name = json |> member "name" |> to_string;
    price = json |> member "price" |> to_int;
    even_amount = json |> member "even_amount" |> to_int;
    odd_amount = json |> member "odd_amount" |> to_int;
  }

let career_from_json json : career_card =
  {
    id = json |> member "id" |> to_int;
    name = json |> member "name" |> to_string;
    salary = json |> member "salary" |> to_int;
    bonus = json |> member "bonus_salary" |> to_int;
  }

let action_cards json =
  let action_pay_list =
    json |> member "action_pay" |> to_list |> List.map action_pay_from_json
  in
  let spin_list =
    json |> member "action_spin" |> to_list |> List.map action_spin_from_json
  in
  let choice_list =
    json |> member "action_choice" |> to_list
    |> List.map action_choice_from_json
  in
  action_pay_list @ spin_list @ choice_list

let house_cards json =
  json |> member "house" |> to_list |> List.map house_from_json

let career_cards json =
  json |> member "career" |> to_list |> List.map career_from_json

let rec draw_action_card_helper lst index =
  let h, t =
    match lst with
    | [] -> failwith "Invalid action card list for drawing"
    | h :: t -> (h, t)
  in
  match index with
  | 0 -> h
  | _ -> draw_action_card_helper t (index - 1)

let draw_action_card l =
  Random.self_init ();
  let range = List.length l in
  let r = Random.int range in
  draw_action_card_helper l r
