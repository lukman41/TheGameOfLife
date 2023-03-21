open Game_of_life
open Game

let main () =
  print_string " Welcome to the game of ";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "L";
  ANSITerminal.print_string [ ANSITerminal.blue ] "I";
  ANSITerminal.print_string [ ANSITerminal.green ] "F";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "E";
  print_string ". ";
  print_endline "Player_1's name: ";
  let player_1 = make_player (read_line ()) None in
  print_endline "Player_2's name:";
  let player_2 = make_player (read_line ()) None in
  print_endline "Player_3's name:";
  let player_3 = make_player (read_line ()) None in
  print_endline "Player_4's name:";
  let player_4 = make_player (read_line ()) None in
  let player_list = [player_1;player_2;player_3;player_4] in 
  ()

let () = main ()
