open Yojson.Basic.Util
open ANSITerminal
open Command
open Cards

let data_dir_prefix = "data" ^ Filename.dir_sep

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

module Cards = struct
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
    requires_degree : bool;
  }

  let action_card_prompt (card : action_cards) =
    match card with
    | Pay pcard -> pcard.prompt
    | Spin scard -> scard.prompt
    | Choice ccard -> ccard.prompt

  let action_card_name (card : action_cards) =
    match card with
    | Pay pcard -> pcard.name
    | Spin scard -> scard.name
    | Choice ccard -> ccard.name

  let pay_card_amount (card : action_cards) =
    match card with
    | Pay pcard -> pcard.amount
    | _ -> failwith "Invalid action card type for pay_card_amount"

  let spin_card_amounts (card : action_cards) =
    match card with
    | Spin s -> (s.even_amount, s.odd_amount)
    | _ -> failwith "Invalid action card type for spin_card_amounts"

  let choice_card_amounts (card : action_cards) =
    match card with
    | Choice s -> (s.a_amount, s.b_amount)
    | _ -> failwith "Invalid action card type for choice_card_amounts"

  let action_from_pay_json json : action_cards =
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
        a_amount = json |> member "a_amount" |> to_int;
        b_amount = json |> member "b_amount" |> to_int;
      }

  let house_from_json json : house_card =
    {
      id = json |> member "id" |> to_int;
      name = json |> member "name" |> to_string;
      price = json |> member "price" |> to_int;
      even_amount = json |> member "spin_even" |> to_int;
      odd_amount = json |> member "spin_odd" |> to_int;
    }

  let career_from_json json : career_card =
    {
      id = json |> member "id" |> to_int;
      name = json |> member "name" |> to_string;
      salary = json |> member "salary" |> to_int;
      bonus = json |> member "bonus_salary" |> to_int;
      requires_degree = json |> member "requires_degree" |> to_bool;
    }

  let action_cards json =
    let action_pay_list =
      json |> member "action_pay" |> to_list |> List.map action_from_pay_json
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

  let rec draw_card_helper lst index =
    let h, t =
      match lst with
      | [] -> failwith "Invalid action card list for drawing"
      | h :: t -> (h, t)
    in
    match index with
    | 0 -> h
    | _ -> draw_card_helper t (index - 1)

  let draw_card l =
    Random.self_init ();
    let range = List.length l in
    let r = Random.int range in
    draw_card_helper l r
end

let cards_json = Yojson.Basic.from_file (data_dir_prefix ^ "cards.json")

(**Need to figure out how to open the json files correctly- Look at a2. Right
   now we are getting errors the way we are doing it *)
let action_cards = Cards.action_cards cards_json

let house_cards = Cards.house_cards cards_json
let career_cards = Cards.career_cards cards_json
let board_json = Yojson.Basic.from_file (data_dir_prefix ^ "board.json")
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

module Board = struct
  (**Takes in a single spot from the json and returns the type of spot as a
     string*)
  let get_spot_type json = json |> member "type" |> to_string

  (** Takes in the board json and outputs it as a list in order with the
      spot_types as strings*)
  let board_from_json json =
    json |> member "board" |> to_list |> List.map get_spot_type

  (**Builds an accumulator for the board as a spot list. Keeps track of the
     previous spot we just worked with in order to have next when making the
     current spot*)
  let rec make_board_helper acc prev string_board =
    let backward_board = List.rev string_board in
    match backward_board with
    | [] -> acc
    | h1 :: h2 :: t ->
        if h1 = "Retire" then
          make_board_helper (retire_spot :: acc) retire_spot (h2 :: t)
        else
          let new_spot = make_spot h1 prev in
          make_board_helper (new_spot :: acc) new_spot (h2 :: t)
    | h :: t ->
        let new_spot = make_spot h prev in
        new_spot :: acc

  (**Makes the board with an empty accumulator *)
  let make_board board = make_board_helper [] retire_spot board

  (**Returns the start of the board *)
  let start_spot board = List.hd board
end

let board_spot_list = Board.board_from_json board_json |> Board.make_board

let rec prompt_for_spin g =
  let p = g.current_player in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (g.current_player.name ^ ", type ");
  ANSITerminal.print_string [ ANSITerminal.magenta ] "'s";
  ANSITerminal.print_string [ ANSITerminal.blue ] "p";
  ANSITerminal.print_string [ ANSITerminal.green ] "i";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "n'";
  ANSITerminal.print_string [ ANSITerminal.blue ] " to spin.";
  print_newline ();

  try
    match Command.parse (read_line ()) with
    | Spin ->
        let player_spin =
          Random.self_init ();
          let r = Random.int 12 in
          if r = 0 || r = 1 then 1 else r - 1
        in
        ANSITerminal.print_string [ ANSITerminal.green ]
          (p.name ^ " spun a " ^ string_of_int player_spin);
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
    | Draw ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a draw command, try "spin" or "quit" |};
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

let rec choose_from_three_cards (c1 : Cards.career_card)
    (c2 : Cards.career_card) (c3 : Cards.career_card) name =
  ANSITerminal.print_string [ ANSITerminal.blue ] "Your three choices are: ";
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.magenta ] "OPTION A: ";
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.cyan ] c1.name;
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    ("Salary: " ^ string_of_int c1.salary);
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    ("Bonus: " ^ string_of_int c1.bonus);
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.magenta ] "OPTION B: ";
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.cyan ] c2.name;
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    ("Salary: " ^ string_of_int c2.salary);
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    ("Bonus: " ^ string_of_int c2.bonus);
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.magenta ] "OPTION C: ";
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.cyan ] c3.name;
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    ("Salary: " ^ string_of_int c3.salary);
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    ("Bonus: " ^ string_of_int c3.bonus);
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.blue ]
    {|Type "choose a", "choose b" or "choose c".|};
  print_newline ();
  try
    match Command.parse (read_line ()) with
    | Choose w -> (
        match String.concat " " w with
        | "a" ->
            ANSITerminal.print_string [ ANSITerminal.blue ]
              "Congrats your new career is  ";
            print_endline c1.name;
            {
              name = c1.name;
              salary = c1.salary;
              bonus_salary = c1.bonus;
              requires_degree = c1.requires_degree;
            }
        | "b" ->
            ANSITerminal.print_string [ ANSITerminal.blue ]
              "Congrats your new career is  ";
            print_endline c2.name;
            {
              name = c2.name;
              salary = c2.salary;
              bonus_salary = c2.bonus;
              requires_degree = c2.requires_degree;
            }
        | "c" ->
            ANSITerminal.print_string [ ANSITerminal.blue ]
              "Congrats your new career is  ";
            print_endline c3.name;
            {
              name = c3.name;
              salary = c3.salary;
              bonus_salary = c3.bonus;
              requires_degree = c3.requires_degree;
            }
        | _ -> raise Malformed)
    | Start ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a start command, try a choose command |};
        choose_from_three_cards c1 c2 c3 name
    | Draw ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a draw command, try a choose command |};
        choose_from_three_cards c1 c2 c3 name
    | Quit -> exit 0
    | Spin ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a spin command, try a choose command |};
        choose_from_three_cards c1 c2 c3 name
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was empty, try "choose a" or something like that |};
      choose_from_three_cards c1 c2 c3 name
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was malformed, try "choose b" or something like that |};
      choose_from_three_cards c1 c2 c3 name

let rec draw_multiple_career_cards name card_list =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (name
   ^ {|, time to pick a career. Type "draw" to draw three career cards to choose from.|}
    );
  print_newline ();
  match Command.parse (read_line ()) with
  | Draw ->
      let c1 = Cards.draw_card card_list in
      let c2 = Cards.draw_card card_list in
      let c3 = Cards.draw_card card_list in
      choose_from_three_cards c1 c2 c3 name
  | Spin ->
      print_endline {| that was a spin command, type "draw" |};
      draw_multiple_career_cards name card_list
  | Choose a ->
      print_endline {| that was a choose command, type "draw" |};
      draw_multiple_career_cards name card_list
  | Quit -> exit 0
  | Start ->
      print_endline {| that was a start command, type "draw" |};
      draw_multiple_career_cards name card_list

let set_player_career c name =
  match c with
  | true -> None
  | false ->
      let non_degree_cards =
        List.filter
          (fun (cc : Cards.career_card) -> cc.requires_degree = false)
          career_cards
      in
      Some (draw_multiple_career_cards name non_degree_cards)

let set_player_money = function
  | true -> 150000
  | false -> 250000

let make_player name choice =
  {
    name;
    career = set_player_career choice name;
    money = set_player_money choice;
    position = Board.start_spot board_spot_list;
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
      g
  | Some career ->
      print_endline
        (g.current_player.name ^ " landed on a payday, their bonus salary is: "
        ^ string_of_int career.bonus_salary);
      pay_current_player g career.bonus_salary

let pass_a_payday g =
  match g.current_player.career with
  | None ->
      print_endline
        (g.current_player.name ^ " passed a payday, but they don't have a job.");
      g
  | Some career ->
      print_endline
        (g.current_player.name ^ " passed a payday, their salary is: "
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
      let updated_player =
        {
          name = g.current_player.name;
          money = g.current_player.money;
          career =
            Some (draw_multiple_career_cards g.current_player.name career_cards);
          position = g.current_player.position;
          houses = g.current_player.houses;
          pegs = g.current_player.pegs;
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
  | Some career -> g

let rec family_stop_op game =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Do you want to have/adopt a child?";
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    {|To make a choice, type "choose" before the choice you want to make- yes or no.|};
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    {|For example: To have/adopt a child enter the phrase: choose yes|};
  print_newline ();
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
        family_stop_op game
    | Start ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a start command, to make a choice type "choose" before the choice you want to enter |};
        family_stop_op game
    | Draw ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a draw command, to make a choice type "choose" before the choice you want to enter |};
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

let rec spin_action_card () : int =
  print_endline {|Type "draw" to draw an action card: |};
  try
    match Command.parse (read_line ()) with
    | Spin -> spin
    | Quit -> exit 0
    | Draw ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a draw command, to draw an action card type "spin" |};
        spin_action_card ()
    | Start ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a start command, to draw an action card type "spin" |};
        spin_action_card ()
    | Choose _ ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a choose command, to draw an action card type "spin" |};
        spin_action_card ()
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was empty, try "spin" |};
      spin_action_card ()
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was malformed, try "spin" |};
      spin_action_card ()

let rec choice_draw_action_helper (card : Cards.action_cards) =
  print_endline "Here are your choices: ";
  print_endline (card |> Cards.action_card_prompt);
  print_endline "Choose option A or option B.";
  print_endline
    {|Type "choose A" or type "choose B" to indicate which option you would like to choose.|};
  try
    match Command.parse (read_line ()) with
    | Choose i -> (
        try
          match i with
          | h :: t ->
              let a, b = Cards.choice_card_amounts card in
              if h = "A" && List.length t = 0 then a
              else if h = "B" && List.length t = 0 then b
              else raise Malformed
          | _ -> raise Malformed
        with Failure _ -> raise Malformed)
    | Quit -> exit 0
    | Spin ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a spin command, to make a choice type "choose" before the choice you want to enter |};
        choice_draw_action_helper card
    | Start ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a start command, to make a choice type "choose" before the choice you want to enter |};
        choice_draw_action_helper card
    | Draw ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a draw command, to make a choice type "choose" before the choice you want to enter |};
        choice_draw_action_helper card
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was empty, try "choose yes" or something like that |};
      choice_draw_action_helper card
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was malformed, try "choose no" or something like that |};
      choice_draw_action_helper card

let rec draw_action_card action_lst g =
  print_endline {|Type "draw" to draw an action card: |};
  try
    match Command.parse (read_line ()) with
    | Draw ->
        let card = Cards.draw_card action_cards in
        let name = Cards.action_card_name card in
        let prompt = Cards.action_card_prompt card in
        print_endline "Here is your card: ";
        print_endline ("Name: " ^ name);
        print_endline ("Prompt: " ^ prompt);

        let player_money =
          match card with
          | Pay p -> g.current_player.money + p.amount
          | Spin s -> (
              match spin_action_card () mod 2 with
              | 0 -> g.current_player.money + s.even_amount
              | _ -> g.current_player.money + s.odd_amount)
          | Choice c -> g.current_player.money + choice_draw_action_helper card
        in
        let updated_current_player =
          {
            name = g.current_player.name;
            money = player_money;
            career = g.current_player.career;
            position = g.current_player.position;
            houses = g.current_player.houses;
            pegs = g.current_player.pegs;
            has_degree = g.current_player.has_degree;
          }
        in
        print_endline
          (updated_current_player.name ^ ", you have "
          ^ string_of_int updated_current_player.money
          ^ " now");
        {
          current_player = updated_current_player;
          active_players = g.active_players;
          retired_players = g.retired_players;
          game_board = g.game_board;
        }
    | Quit -> exit 0
    | Spin ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a spin command, to draw an action card type "draw" |};
        draw_action_card action_lst g
    | Start ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a start command, to draw an action card type "draw" |};
        draw_action_card action_lst g
    | Choose _ ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a choose command, to draw an action card type "draw" |};
        draw_action_card action_lst g
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was empty, try "choose yes" or something like that |};
      draw_action_card action_lst g
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was malformed, try "choose no" or something like that |};
      draw_action_card action_lst g

let career_stop_op game =
  match game.current_player.has_degree with
  | true -> failwith "todo"
  | false -> failwith "todo"

let rec married_stop_op game =
  let player = game.current_player in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (player.name ^ ", Do you want to get married?");
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    {|To make a choice, type "choose" before the choice you want to make- yes or no.|};
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    {|For example: If you do not want to get married enter the phrase: choose no|};
  print_newline ();
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
          {|That was a spin command, to make a choice, type "choose" before the choice you want to enter |};
        married_stop_op game
    | Start ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a start command, to make a choice, type "choose" before the choice you want to enter |};
        married_stop_op game
    | Draw ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a draw command, to make a choice, type "choose" before the choice you want to enter |};
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

let rec sell_house_op (house : house) game =
  let new_house_list =
    List.filter
      (fun (h : house) -> h.name <> house.name)
      game.current_player.houses
  in
  print_endline "Spin to see what price your house will sell for.";
  print_endline
    ("If your spin number is even, your house will sell for "
    ^ string_of_int house.sell_even);
  print_endline
    ("And if your spin number is odd, your house will sell for "
    ^ string_of_int house.sell_odd);
  let spin = prompt_for_spin game in
  let even_odd = if spin mod 2 = 0 then house.sell_even else house.sell_odd in
  let updated_player =
    {
      name = game.current_player.name;
      money = game.current_player.money + even_odd;
      career = game.current_player.career;
      position = game.current_player.position;
      houses = new_house_list;
      pegs = game.current_player.pegs;
      has_degree = game.current_player.has_degree;
    }
  in
  let updated_game =
    {
      current_player = updated_player;
      active_players = updated_player :: List.tl game.active_players;
      retired_players = game.retired_players;
      game_board = game.game_board;
    }
  in
  updated_game

let rec check_all_houses (house_list : house list) game =
  if List.length house_list > 0 then
    match house_list with
    | [] -> game
    | house :: rest -> (
        print_endline ("Name: " ^ house.name);
        print_endline ("Price: " ^ string_of_int house.purchase_price);
        print_endline ("Even: " ^ string_of_int house.sell_even);
        print_endline ("Odd: " ^ string_of_int house.sell_odd);
        print_endline "Would you like to sell this house? Yes/No.";
        try
          match Command.parse (read_line ()) with
          | Choose i -> (
              try
                match i with
                | h :: t ->
                    if h = "yes" && List.length t = 0 then
                      sell_house_op house game
                    else if h = "no" && List.length t = 0 then
                      check_all_houses rest game
                    else raise Malformed
                | _ -> raise Malformed
              with Failure _ -> raise Malformed)
          | Quit -> exit 0
          | Spin ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                {|That was a spin command, to make a choice, type "choose" before the choice you want to enter |};
              check_all_houses house_list game
          | Start ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                {|That was a start command, to make a choice, type "choose" before the choice you want to enter |};
              check_all_houses house_list game
          | Draw ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                {|That was a draw command, to make a choice, type "choose" before the choice you want to enter |};
              check_all_houses house_list game
        with
        | Empty ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              {|That command was empty, try "choose yes" or something like that |};
            check_all_houses house_list game
        | Malformed ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              {|That command was malformed, try "choose no" or something like that |};
            check_all_houses house_list game)
  else game

let rec buy_house_op house game =
  try
    match Command.parse (read_line ()) with
    | Choose i -> (
        try
          match i with
          | h :: t ->
              if h = "yes" && List.length t = 0 then
                let updated_player =
                  {
                    name = game.current_player.name;
                    money = game.current_player.money - house.purchase_price;
                    career = game.current_player.career;
                    position = game.current_player.position;
                    houses = house :: game.current_player.houses;
                    pegs = game.current_player.pegs;
                    has_degree = game.current_player.has_degree;
                  }
                in
                let updated_game =
                  {
                    current_player = updated_player;
                    active_players =
                      updated_player :: List.tl game.active_players;
                    retired_players = game.retired_players;
                    game_board = game.game_board;
                  }
                in
                updated_game
              else if h = "no" && List.length t = 0 then
                check_all_houses game.current_player.houses game
              else raise Malformed
          | _ -> raise Malformed
        with Failure _ -> raise Malformed)
    | Quit -> exit 0
    | Spin ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a spin command, to make a choice, type "choose" before the choice you want to enter |};
        buy_house_op house game
    | Start ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a start command, to make a choice, type "choose" before the choice you want to enter |};
        buy_house_op house game
    | Draw ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a draw command, to make a choice, type "choose" before the choice you want to enter |};
        buy_house_op house game
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was empty, try "choose yes" or something like that |};
      buy_house_op house game
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was malformed, try "choose no" or something like that |};
      buy_house_op house game

let rec landed_house_op game =
  try
    match Command.parse (read_line ()) with
    | Draw ->
        let house_card = Cards.draw_card house_cards in
        print_endline ("Name: " ^ house_card.name);
        print_endline ("Price: " ^ string_of_int house_card.price);
        print_endline ("Even: " ^ string_of_int house_card.even_amount);
        print_endline ("Odd: " ^ string_of_int house_card.odd_amount);
        print_newline ();
        print_endline "Would you like to purchase this house? Yes/No.";
        print_endline
          {|to make a choice, type "choose" before the choice you want to make|};
        let house =
          {
            name = house_card.name;
            purchase_price = house_card.price;
            sell_even = house_card.even_amount;
            sell_odd = house_card.odd_amount;
          }
        in
        buy_house_op house game
    | Quit -> exit 0
    | Spin ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a spin command, to draw a house card, type "draw" |};
        landed_house_op game
    | Start ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a start command, to draw a house card, type "draw" |};
        landed_house_op game
    | Choose _ ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a choose command, to draw a house card, type "draw" |};
        landed_house_op game
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was empty, try "draw"  |};
      landed_house_op game
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was malformed, try "draw"  |};
      landed_house_op game

let crisis_stop_op g =
  print_endline
    "You have reached a Crisis Stop! You're currently going through a \n\n\
    \                mid-Life crisis, and you are considering making changes \
     to your life.";
  print_endline
    "You are able to change your name, sell your house, or change your career";
  print_endline {|To change your name, type "change name"|};
  print_endline {|To sell your house, type "change change"|};
  print_endline {|To change your career, type "change career"|};
  g

let landed_spot_operations g =
  (*all functions should return updated game.t*)
  match g.current_player.position with
  | Start _ -> failwith "unimplemented"
  | Retire _ ->
      move_player_to_retired g
      (*function to take player out of game so they can wait for other players
        to finish*)
  | Payday _ ->
      land_on_payday g
      |> switch_active_player (*funtion to pay players their bonus salary*)
  | Action _ -> draw_action_card Cards.action_cards g |> switch_active_player
  | MarriedStop { next } -> married_stop_op g |> switch_active_player
  | FamilyStop { next } -> family_stop_op g |> switch_active_player
  | CrisisStop { next } ->
      failwith "unimplemented" (*function to perform stop choice*)
  | GraduationStop { next } ->
      graduation_stop_operation g |> switch_active_player
      (* function to perform stop choice and check if you graduated *)
  | House _ ->
      landed_house_op g |> switch_active_player
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
  | Retire _ -> move_player_to_retired g
  | Payday _ ->
      let g = pass_a_payday g in
      move_helper g (spin_number - 1)
  | MarriedStop { next } ->
      let g = married_stop_op g in
      move_helper g (spin_number - 1)
  | FamilyStop { next } ->
      let g = family_stop_op g in
      move_helper g (spin_number - 1)
  | CrisisStop { next } ->
      failwith "unimplemented" (*function to perform stop choice*)
  | GraduationStop { next } ->
      let g = graduation_stop_operation g in
      move_helper g (spin_number - 1)
  | _ -> move_helper g (spin_number - 1)
(*Since you dont do anything when you pass the rest of the spots,only when you
  land on them, we can just call the helper with the player moved one spot over
  and one less spot to go*)

let move_current_player g i = move_helper g i

let end_game g =
  let lst_players = g.retired_players in
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

let rec string_player_order acc num = function
  | [] -> ANSITerminal.print_string [ ANSITerminal.green ] (acc ^ ".")
  | h :: t ->
      if num = 0 then string_player_order (acc ^ h.name) 1 t
      else string_player_order (acc ^ "-> " ^ h.name) (num + 1) t

let first_turn_spin players =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "Take turns spinning the wheel. The highest spin will go first.";
  print_newline ();
  let rec prompt_players p assoc_spun_lst =
    match p with
    | [] -> find_max players assoc_spun_lst
    | h :: t -> (
        ANSITerminal.print_string [ ANSITerminal.blue ] (h.name ^ ", type ");
        ANSITerminal.print_string [ ANSITerminal.magenta ] "'s";
        ANSITerminal.print_string [ ANSITerminal.blue ] "p";
        ANSITerminal.print_string [ ANSITerminal.green ] "i";
        ANSITerminal.print_string [ ANSITerminal.yellow ] "n'";
        ANSITerminal.print_string [ ANSITerminal.blue ] " to spin.";
        print_newline ();
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
        | "quit" -> exit 0
        | _ ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "That was the incorrect command, please try again.";
            print_newline ();
            prompt_players p assoc_spun_lst)
  in
  let current_player, active_players = prompt_players players [] in
  ANSITerminal.print_string [ ANSITerminal.green ]
    (current_player.name ^ " is going first! ");
  string_player_order "The order is " 0 active_players;
  print_newline ();
  {
    current_player;
    active_players;
    retired_players = [];
    game_board = board_spot_list;
  }

let active_players g = g.active_players
let current_player g = g.current_player
let current_player_name p = p.name
