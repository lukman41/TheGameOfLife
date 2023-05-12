type object_phrase = string list

type command =
  | Quit
  | Spin
  | Start
  | Choose of object_phrase
  | Draw
  | Change of object_phrase

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
      | "draw", [] -> Draw
      | "spin", [] -> Spin
      | "quit", [] -> Quit
      | "start", [] -> Start
      | "choose", [] -> raise Empty
      | "choose", _ -> Choose t
      | "change", [] -> raise Empty
      | "change", _ -> Change t
      | _, _ -> raise Malformed)
