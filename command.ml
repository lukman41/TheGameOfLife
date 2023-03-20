type object_phrase = string list

type command =
  | Quit
  | Spin
  | Choose of object_phrase

exception Empty
exception Malformed

let parse str =
  match
    List.filter
      (fun ele -> String.length ele > 0)
      (String.split_on_char ' ' str)
  with
  | [] -> raise Empty
  | h :: t -> (
      match (h, t) with
      | "spin", [] -> Spin
      | "quit", [] -> Quit
      | "choose", [] -> raise Empty
      | "choose", _ -> Choose t
      | _, _ -> raise Malformed)
