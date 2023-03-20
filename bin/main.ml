let main () = 
   print_string " Welcome to the game of ";
  ANSITerminal.print_string [ANSITerminal.magenta] "L";
  ANSITerminal.print_string [ANSITerminal.blue] "I";
  ANSITerminal.print_string [ANSITerminal.green] "F";
  ANSITerminal.print_string [ANSITerminal.yellow] "E";
  print_string ". "



let () = main ()