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

(* Our types house and career are sort of what I think the card types will be
   like, so it might be smarter to move those inside this module and just access
   them using Card.career or Card.house in the game module *)
