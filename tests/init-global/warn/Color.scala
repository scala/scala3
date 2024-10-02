enum Color:
  case None, White, Black

enum Player:
  case Black, White

  val color: Color =
    if this == Player.Black  // warn
    then Color.Black
    else Color.White
