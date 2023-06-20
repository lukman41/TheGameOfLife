open Game_of_life
open Game
open Command


let rec play g =
  (*this function should take in a game state

    the "base case" is when each player's position is Retire, or when the active
    player list is empty, at that point, this can call end game*)
  match Game.active_players g with
  | [] -> end_game g
  | _ -> play (move_current_player g (Game.prompt_for_spin g))

(*the recursive case is a call to move_current_player with the game state, after
  that call, change the current player to the next player in line and call play
  again. The tricky part here is what to do when one player retires and the
  other players do not, i think we could take that player out of the player
  list, but that will cause problems down the road. *)

(* decided to make move_current_player handle the switching of players *)

let rec number_of_players_prompt () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Enter the number of players:  (Between 2 -4)";
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    {|To make a choice type "choose" before the number you want to enter|};
  print_newline ();

  ANSITerminal.print_string [ ANSITerminal.yellow ]
    {|For example: To play with 3 players, enter the phrase: choose 3|};
  print_newline ();
  try
    match Command.parse (read_line ()) with
    | Choose i -> (
        try
          match int_of_string (String.concat " " i) with
          | 2 | 3 | 4 -> int_of_string (String.concat " " i)
          | _ -> raise Malformed
        with Failure _ -> raise Malformed)
    | Quit -> exit 0
    | Spin ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a spin command, to make a choice type "choose" before the number you want to enter |};
        print_newline ();
        number_of_players_prompt ()
    | Start ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a start command, to make a choice type "choose" before the number you want to enter |};
        print_newline ();
        number_of_players_prompt ()
    | Draw ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a draw command, to make a choice type "choose" before the number you want to enter |};
        number_of_players_prompt ()
    | Change _ ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a change command, to make a choice type "choose" before the number you want to enter |};
        number_of_players_prompt ()
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was empty, please try again.  |};
      print_newline ();
      number_of_players_prompt ()
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was malformed, please try again. |};
      print_newline ();
      number_of_players_prompt ()

let rec player_name_prompt i =
  try
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ("Player_" ^ string_of_int i ^ "'s name: ");
    print_newline ();
    let input = read_line () in
    if input = "" then raise Empty else if input = "quit" then exit 0 else input
  with Empty ->
    ANSITerminal.print_string [ ANSITerminal.red ] {|That name was empty.|};
    player_name_prompt i

let rec go_to_college_prompt name =
  (* uses Command.mli to prompt the player with the name they just used on if
     they want to go college or not. *)
  ANSITerminal.print_string [ ANSITerminal.blue ]
    (name ^ {|, do you want to go to college?|});
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    {|Remember that if you choose to go to college, you will be eligible for more jobs but you must pay a $100,000 tuition fee.|};
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.blue ]
    {|Type "choose yes" or "choose no"|};
  print_newline ();
  try
    match Command.parse (read_line ()) with
    | Choose i -> (
        match String.concat " " i with
        | "yes" -> true
        | "no" -> false
        | _ -> raise Malformed)
    | Quit -> exit 0
    | Spin ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a spin command, to make a choice type "choose" before the choice you want to make |};
        print_newline ();
        go_to_college_prompt name
    | Start ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a start command, to make a choice type "choose" before the choice you want to make |};
        print_newline ();
        go_to_college_prompt name
    | Draw ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a draw command, to make a choice type "choose" before the choice you want to make |};
        go_to_college_prompt name
    | Change _ ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a change command, to make a choice type "choose" before the choice you want to make |};
        go_to_college_prompt name
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was empty, please try again. |};
      print_newline ();
      go_to_college_prompt name
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was malformed, please try again.  |};
      print_newline ();
      go_to_college_prompt name

let rec prompt_for_name_and_college acc i =
  (* takes in a list acc and int i and prompts each player for their name and
     choice in college, returning a list of string*bool tuples.*)
  match i with
  | 0 -> acc
  | _ ->
      let name = player_name_prompt i in
      prompt_for_name_and_college
        ((name, go_to_college_prompt name) :: acc)
        (i - 1)

let player_selection () =
  List.map
    (fun (s, b) -> make_player s b)
    (prompt_for_name_and_college [] (number_of_players_prompt ()))

let rec press_start () =
  ANSITerminal.print_string [ ANSITerminal.blue ] "Type ";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "s";
  ANSITerminal.print_string [ ANSITerminal.blue ] "t";
  ANSITerminal.print_string [ ANSITerminal.green ] "a";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "r";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "t";
  ANSITerminal.print_string [ ANSITerminal.blue ] " to begin the game: ";
  print_newline ();
  try
    match Command.parse (read_line ()) with
    | Start -> ()
    | Quit -> exit 0
    | Spin ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a spin command, try again.|};
        print_newline ();
        press_start ()
    | Choose i ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a choose command, try again.|};
        print_newline ();
        press_start ()
    | Draw ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a draw command, try again.|};
        press_start ()
    | Change _ ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a change command, try again.|};
        press_start ()
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That was an empty command, try again.|};
      print_newline ();
      press_start ()
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That was a malformed command, try again.|};
      print_newline ();
      press_start ()

let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    " \n\nWelcome to The Game of ";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "L";
  ANSITerminal.print_string [ ANSITerminal.blue ] "I";
  ANSITerminal.print_string [ ANSITerminal.green ] "F";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "E";
  print_endline "! ";
  (* Maybe add instructions on how to play? *)
  press_start ();
  let game = first_turn_spin (player_selection ()) in
  let winner = play game in
  ANSITerminal.print_string [ ANSITerminal.magenta ] "W";
  ANSITerminal.print_string [ ANSITerminal.blue ] "I";
  ANSITerminal.print_string [ ANSITerminal.green ] "N";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "N";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "E";
  ANSITerminal.print_string [ ANSITerminal.blue ] "R";
  print_string ("!: " ^ winner ^ " ")

let () = main ()
