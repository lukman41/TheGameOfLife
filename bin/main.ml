open Game_of_life
open Game
open Command

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
          {|to make a choice type "choose" before the number you want to enter |};
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

let rec prompt_for_name_and_college l = failwith "todo"
(* rn i have an int of how many players are in the game, need to build up a list of tuples containing the player name and the  choice of college or not. *)



let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    " \n\nWelcome to The Game of ";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "L";
  ANSITerminal.print_string [ ANSITerminal.blue ] "I";
  ANSITerminal.print_string [ ANSITerminal.green ] "F";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "E";
  print_endline "! ";
  print_endline "Player_1's name: ";
  let player_1 = make_player (read_line ()) None in
  print_endline "Player_2's name:";
  let player_2 = make_player (read_line ()) None in
  print_endline "Player_3's name:";
  let player_3 = make_player (read_line ()) None in
  print_endline "Player_4's name:";
  let player_4 = make_player (read_line ()) None in
  let player_list = [ player_1; player_2; player_3; player_4 ] in
  print_endline "Take turns spinning the wheel. The highest spin will go first.";
  let game = first_turn_spin player_list in
  let game2 = player_payday game 2000 in
  let winner = end_game game2 in
  ANSITerminal.print_string [ ANSITerminal.magenta ] "W";
  ANSITerminal.print_string [ ANSITerminal.blue ] "I";
  ANSITerminal.print_string [ ANSITerminal.green ] "N";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "N";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "E";
  ANSITerminal.print_string [ ANSITerminal.blue ] "R";
  print_string ("!: " ^ winner ^ " ")
(* Welcome message

   instructions of what type of commands you can make

   type "start" to start

   prompt for number of players

   prompt each player for that number of players inputted to enter their name
   and whether they would like to go to college or not

   run first turn spin to determine order

   run play *)

let () = main ()
