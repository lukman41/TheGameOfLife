(** Parsing of player commands. *)

type object_phrase = string list
(** The type [object_phrase] represents the object phrase that can be part of a
    player command. Each element of the list represents a word of the object
    phrase, where a "word" is defined as a consecutive sequence of non-space
    characters. Thus, no element of the list should contain any leading,
    internal, or trailing spaces. The list is in the same order as the words in
    the original player command. For example:

    - If the player command is ["quit     "], then the object phrase is [Quit].

    - If the player command is ["  spin     "], then the object phrase is
      [Spin].

    -If the player command is ["start " ], then the object phrase is [Start]

    - If the player command is [" choose    doctor    "], the the object phrase
      is [Choose \["doctor"\]] *)

(** The type [command] represents a player command that is decomposed into a
    verb and possibly an object phrase. *)

type command =
  | Quit
  | Spin
  | Start
  | Choose of object_phrase

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes
    the verb. Examples:

    - [parse "quit"] is [Quit].
    - [parse "start"] is [Start].
    - [parse "choose Doctor"] is [Choose \["Doctor"\]]
    - [parse "spin"] is [Spin].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed. A command is malformed if
    the verb is not "spin", or "quit", if the verb is "quit" or "spin" and there
    is a non-empty object phrase. *)
