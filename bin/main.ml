open Game_of_life
open Game

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
  print_string " Type '";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "s";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "p";
  ANSITerminal.print_string [ ANSITerminal.blue ] "i";
  ANSITerminal.print_string [ ANSITerminal.green ] "n";
  print_string "' to start. "

let () = main ()
