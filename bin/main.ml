open Game_of_life
open Game
open Command
open Board

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

let rec play g =
  (*this function should take in a game state

    the "base case" is when each player's position is Retire, or when the active
    player list is empty, at that point, this can call end game*)
  match Game.active_players g with
  | [] -> end_game g
  | _ -> play (move_current_player g (prompt_for_spin g))

(*the recursive case is a call to move_current_player with the game state, after
  that call, change the current player to the next player in line and call play
  again. The tricky part here is what to do when one player retires and the
  other players do not, i think we could take that player out of the player
  list, but that will cause problems down the road. *)

(* decided to make move_current_player handle the switching of players *)

let rec number_of_players_prompt () =
  print_endline "Enter the number of players:  (Between 1 -4)";
  print_endline
    {|to make a choice type "choose" before the number you want to enter|};
  try
    match Command.parse (read_line ()) with
    | Choose i -> (
        try
          match int_of_string (String.concat " " i) with
          | 1 | 2 | 3 | 4 -> int_of_string (String.concat " " i)
          | _ -> raise Malformed
        with Failure _ -> raise Malformed)
    | Quit -> exit 0
    | Spin ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a spin command, to make a choice type "choose" before the number you want to enter |};
        number_of_players_prompt ()
    | Start ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a start command, to make a choice type "choose" before the number you want to enter |};
        number_of_players_prompt ()
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was empty, try " choose 4 " or something like that |};
      number_of_players_prompt ()
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was malformed, try "choose 4" or something like that |};
      number_of_players_prompt ()

let rec player_name_prompt i =
  try
    print_endline ("Player_" ^ string_of_int i ^ "'s name: ");
    let input = read_line () in
    if input = "" then raise Empty else input
  with Empty ->
    ANSITerminal.print_string [ ANSITerminal.red ] {|That name was empty.|};
    player_name_prompt i

let rec go_to_college_prompt name =
  (* uses Command.mli to prompt the player with the name they just used on if
     they want to go college or not. *)
  print_endline
    (name
   ^ {|, do you want to go to college, or start your career? Type "choose yes" or "choose no"|}
    );
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
        go_to_college_prompt name
    | Start ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a start command, to make a choice type "choose" before the choice you want to make |};
        go_to_college_prompt name
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was empty, try " choose yes " or something like that |};
      go_to_college_prompt name
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That command was malformed, try "choose no" or something like that |};
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

(* let player_selection () = List.map (fun (s, b) -> make_player s b)
   (prompt_for_name_and_college [] (number_of_players_prompt ())) *)

let rec press_start () =
  print_string "Type ";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "s";
  ANSITerminal.print_string [ ANSITerminal.blue ] "t";
  ANSITerminal.print_string [ ANSITerminal.green ] "a";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "r";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "t";
  print_string " to begin the game: ";
  try
    match Command.parse (read_line ()) with
    | Start -> ()
    | Quit -> exit 0
    | Spin ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a spin command, try again.|};
        press_start ()
    | Choose i ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          {|That was a choose command, try again.|};
        press_start ()
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That was an empty command, try again.|};
      press_start ()
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        {|That was a malformed command, try again.|};
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
  press_start ()
(* let game = first_turn_spin (player_selection ()) in let winner = play game in
   ANSITerminal.print_string [ ANSITerminal.magenta ] "W";
   ANSITerminal.print_string [ ANSITerminal.blue ] "I";
   ANSITerminal.print_string [ ANSITerminal.green ] "N";
   ANSITerminal.print_string [ ANSITerminal.yellow ] "N";
   ANSITerminal.print_string [ ANSITerminal.magenta ] "E";
   ANSITerminal.print_string [ ANSITerminal.blue ] "R"; print_string ("!: " ^
   winner ^ " ") *)

let () = main ()
