type object_phrase = string list

type command =
  | Quit
  | Spin

exception Empty
exception Malformed

let parse str = 
  match List.filter (fun ele -> String.length ele > 0) (String.split_on_char ' ' str) with
  | [] -> raise Empty
  | h::t -> if h = "quit" && List.length t = 0 then Quit 
  else if h = "spin" && List.length t = 0 
    then Spin else 
        raise Malformed
