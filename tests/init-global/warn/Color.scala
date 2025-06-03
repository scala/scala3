enum Color:
  case None, White, Black

enum Player:
  case Black, White

  // Explanation: See the desugaring below
  val color: Color =
    if this == Player.Black  // warn
    then Color.Black
    else Color.White

// From the desugaring of Player, we can see the field `Player.Black` is not yet
// initialized during evaluation of the first `new Player`:
//
//     class Player:
//         val color: Color =
//            if this == Player.Black ...
//
//     object Player:
//          val Black: Player = new Player     // <--- problem
//          val White: Player = new Player
//
//
//  The complex desugaring makes it difficult to see the initialization
//  semantics and it is prone to make such hard-to-spot mistakes.
//
// Note: The desugaring above is simplified for presentation.
