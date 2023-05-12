open Yojson.Basic.Util
open ANSITerminal
open Command
open Cards

type spot =
  | Start of { next : spot option }
  | Retire of { next : spot option }
  | Payday of { next : spot option }
  | Action of { next : spot option }
  | MarriedStop of { next : spot option }
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

let card_json = Yojson.Basic.from_file "cards.json"
let board_json = Yojson.Basic.from_file "board.json"

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

let action_card = Cards.action_cards card_json
let house_cards = Cards.house_cards card_json
let career_cards = Cards.career_cards card_json

let get_next_position pos =
  match pos with
  | Start { next } -> next
  | Retire { next } -> failwith "tried to move from retire spot"
  | Payday { next } -> next
  | Action { next } -> next
  | MarriedStop { next } -> next
  | FamilyStop { next } -> next
  | CrisisStop { next } -> next
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

let move_player_to_retired game =
  let new_list_players =
    List.filter (fun x -> x <> game.current_player) game.active_players
  in
  let new_relist_players = [ game.current_player ] in
  {
    current_player = List.hd new_list_players;
    active_players = new_list_players;
    retired_players = game.retired_players @ new_relist_players;
    game_board = game.game_board;
  }

let switch_active_player g =
  match g.active_players with
  | [] -> g
  | old_current_player :: rest_of_players -> (
      match rest_of_players with
      | [] -> g
      | next_player_up :: remaining_players ->
          {
            current_player = next_player_up;
            active_players = remaining_players @ [ old_current_player ];
            retired_players = g.retired_players;
            game_board = g.game_board;
          })

let pay_current_player g amt =
  let updated_player =
    {
      name = g.current_player.name;
      money = g.current_player.money + amt;
      career = g.current_player.career;
      position = g.current_player.position;
      houses = g.current_player.houses;
      pegs = g.current_player.pegs;
      has_degree = g.current_player.has_degree;
    }
  in
  {
    current_player = updated_player;
    active_players = updated_player :: List.tl g.active_players;
    retired_players = g.retired_players;
    game_board = g.game_board;
  }

let land_on_payday g =
  match g.current_player.career with
  | None ->
      print_endline
        (g.current_player.name
       ^ " landed on a payday, but they don't have a job.");
      switch_active_player g
  | Some career ->
      print_endline
        (g.current_player.name ^ " landed on a payday, their bonus salary is: "
        ^ string_of_int career.bonus_salary);
      pay_current_player g career.bonus_salary |> switch_active_player

let pass_a_payday g =
  match g.current_player.career with
  | None ->
      print_endline
        (g.current_player.name ^ " passed a payday, but they don't have a job.");
      switch_active_player g
  | Some career ->
      print_endline
        (g.current_player.name ^ " passed a paydat, their salary is: "
        ^ string_of_int career.salary);
      pay_current_player g career.salary

let add_pegs g amt =
  let updated_player =
    {
      name = g.current_player.name;
      money = g.current_player.money;
      career = g.current_player.career;
      position = g.current_player.position;
      houses = g.current_player.houses;
      pegs = g.current_player.pegs + amt;
      has_degree = g.current_player.has_degree;
    }
  in
  let updated_game =
    {
      current_player = updated_player;
      active_players = updated_player :: List.tl g.active_players;
      retired_players = g.retired_players;
      game_board = g.game_board;
    }
  in
  updated_game

let graduation_stop_operation g =
  match g.current_player.career with
  | None ->
      print_endline "You Graduated. Now Pick a career!";
      failwith "todo function to draw career cards"
  | Some career -> g

let landed_spot_operations g =
  (*all functions should return updated game.t*)
  match g.current_player.position with
  | Start _ -> failwith "unimplemented"
  | Retire _ ->
      move_player_to_retired g
      (*function to take player out of game so they can wait for other players
        to finish*)
  | Payday _ -> land_on_payday g (*funtion to pay players their bonus salary*)
  | Action _ -> failwith "unimplemented" (*function to draw action card*)
  | MarriedStop { next } ->
      failwith "unimplemented" (*function to perform stop choice*)
  | FamilyStop { next } ->
      failwith "unimplemented" (*function to perform stop choice*)
  | CrisisStop { next } ->
      failwith "unimplemented" (*function to perform stop choice*)
  | GraduationStop { next } ->
      graduation_stop_operation g |> switch_active_player
      (* function to perform stop choice and check if you graduated *)
  | House _ ->
      failwith "unimplemented"
      (*function to draw a house card ask if player wants to buy, and *)
  | Friend _ ->
      add_pegs g 1
      |> switch_active_player (*function to perform add peg choice*)
  | Pet _ ->
      add_pegs g 1
      |> switch_active_player (*function to perform add peg choice*)
  | Baby _ ->
      add_pegs g 1
      |> switch_active_player (*function to perform add peg choice*)
  | Twins _ ->
      add_pegs g 2
      |> switch_active_player (*function to perform add peg choice*)
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
      move_player_to_retired g
      (*function to take player out of game so they can wait for other players
        to finish*)
  | Payday _ ->
      let g = pass_a_payday g in
      move_helper g (spin_number - 1)
      (*function to pay players their bonus salary*)
  | MarriedStop { next } ->
      failwith "unimplemented" (*function to perform stop choice*)
  | FamilyStop { next } ->
      failwith "unimplemented" (*function to perform stop choice*)
  | CrisisStop { next } ->
      failwith "unimplemented" (*function to perform stop choice*)
  | GraduationStop { next } ->
      let g = graduation_stop_operation g in
      move_helper g (spin_number - 1)
      (* function to perform stop and check if you graduated *)
  | _ -> move_helper g (spin_number - 1)
(*Since you dont do anything when you pass the rest of the spots,only when you
  land on them, we can just call the helper with the player moved one spot over
  and one less spot to go*)

  let print_endline_prompt s = print_endline s
  let rec family_stop_op game = 
    print_endline_prompt "Choose whether or not you want to have/adopt a child with either yes or no.";
    print_endline {|to make a choice, type "choose" before the choice you want to make|};
    try
      match Command.parse (read_line ()) with
      | Choose i -> (
          try
            match i with
            | h :: t -> if h = "yes" && List.length t = 0 then add_pegs game 1 
              else if h = "no" && List.length t = 0 then game else raise Malformed
            | _ -> raise Malformed
          with Failure _ -> raise Malformed)
      | Quit -> exit 0
      | Spin ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            {|That was a spin command, to make a choice type "choose" before the choice you want to enter |};
          family_stop_op game
      | Start ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            {|That was a start command, to make a choice type "choose" before the choice you want to enter |};
          family_stop_op game
    with
    | Empty ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That command was empty, try "choose yes" or something like that |};
        family_stop_op game
    | Malformed ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That command was malformed, try "choose no" or something like that |};
        family_stop_op game
let rec married_stop_op game =
  print_endline
    "Choose whether or not you want to get married with either yes or no.";
  print_endline
    {|to make a choice, type "choose" before the choice you want to make|};
  try
    match Command.parse (read_line ()) with
    | Choose i -> (
        try
          match i with
          | h :: t ->
              if h = "yes" && List.length t = 0 then add_pegs game 1
              else if h = "no" && List.length t = 0 then game
              else raise Malformed
          | _ -> raise Malformed
        with Failure _ -> raise Malformed)
    | Quit -> exit 0
    | Spin ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a spin command, to make a choice type "choose" before the choice you want to enter |};
        married_stop_op game
    | Start ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a start command, to make a choice type "choose" before the choice you want to enter |};
        married_stop_op game
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was empty, try "choose yes" or something like that |};
      married_stop_op game
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was malformed, try "choose no" or something like that |};
      married_stop_op game

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

let rec prompt_for_spin g =
  let p = Game.current_player g in
  Stdlib.print_string (Game.current_player_name p ^ ", type ");
  ANSITerminal.print_string [ ANSITerminal.magenta ] "'s";
  ANSITerminal.print_string [ ANSITerminal.blue ] "p";
  ANSITerminal.print_string [ ANSITerminal.green ] "i";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "n'";
  print_endline " to spin.";
  try
    match Command.parse (read_line ()) with
    | Spin ->
        let player_spin =
          Random.self_init ();
          let r = Random.int 12 in
          if r = 0 || r = 1 then 1 else r - 1
        in
        ANSITerminal.print_string [ ANSITerminal.green ]
          (Game.current_player_name p ^ " spun a " ^ string_of_int player_spin);
        print_newline ();
        player_spin
    | Quit -> exit 0
    | Choose _ ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That Command was a choose, try "spin" or "quit" |};
        prompt_for_spin g
    | Start ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a start command, try "spin" or "quit" |};
        prompt_for_spin g
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That Command was empty, try "spin" or "quit" |};
      print_newline ();
      prompt_for_spin g
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That Command was malformed, try try "spin" or "quit" |};
      print_newline ();
      prompt_for_spin g

let retire_spot = Retire { next = None }

(** Constructs a Spot object with the spot_type and the next_spot *)
let make_spot spot_type (next_spot : spot) =
  match spot_type with
  | "StartCollege" -> Start { next = Some next_spot }
  | "Payday" -> Payday { next = Some next_spot }
  | "GraduationStop" -> GraduationStop { next = Some next_spot }
  | "Action" -> Action { next = Some next_spot }
  | "Career" -> Career { next = Some next_spot }
  | "MarriedStop" -> MarriedStop { next = Some next_spot }
  | "Pet" -> Pet { next = Some next_spot }
  | "House" -> House { next = Some next_spot }
  | "Friend" -> Friend { next = Some next_spot }
  | "FamilyStop" -> FamilyStop { next = Some next_spot }
  | "Baby" -> Baby { next = Some next_spot }
  | "Twins" -> Twins { next = Some next_spot }
  | "CrisisStop" -> CrisisStop { next = Some next_spot }
  | "Retire" -> retire_spot
  | _ -> failwith "unimplemented"
