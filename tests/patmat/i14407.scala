// scalac: -Werror
@main def Test =
  val it: Int = 42
  42L match
    case `it` => println("pass")
    case _    => println("fail")
