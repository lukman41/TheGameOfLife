open Game
(* this is where I think we can define all the cards we need, action,house,etc
   and store them in a red-black tree dictonary (for more lines of code ) we can
   map each card to a number and to draw cards just run find of a random number
   in bewtween 1 and the # of cards. *)

(** Card.t is the type of card which we can map to a number, each card should
    come with a name and prompt to print out, some card types need a function
    that takes in a Game.t and returns a Game.t. For example career cards
    individually do not need a function, but we will need a function that draws
    three of them randomly and allows the player to choose one. On the other
    hand, after drawing an action card, it might need a function attached to it
    that prompts the player and then makes changes to the player. For a house
    card, we need a function to draw one, and let the player buy and sell them*)

(* changed my mind they don't need individual functions, since they mostly all
   do the same thing. For example one type of action card just takes or gives
   you money so that is one function we can use for all those cards. Then
   another type of action card gives you a choice, so we could make a function
   that reads the choice and calls functions based on that. *)
(* Our types house and career are sort of what I think the card types will be
   like, so it might be smarter to move those inside this module and just access
   them using Card.career or Card.house in the game module *)

type action_cards
type house_card
type career_card

val action_from_pay_json : Yojson.Basic.t -> action_cards
val action_spin_from_json : Yojson.Basic.t -> action_cards
val action_choice_from_json : Yojson.Basic.t -> action_cards
val house_from_json : Yojson.Basic.t -> house_card
val career_from_json : Yojson.Basic.t -> career_card
val action_cards : Yojson.Basic.t -> action_cards list
val house_cards : Yojson.Basic.t -> house_card list
val career_cards : Yojson.Basic.t -> career_card list
val draw_card : 'a list -> 'a
