// scalac: -Werror
@main def Test =
  val it: Int = 42
  42L match
    case `it` => println("pass")
    case `it` => println("dupe") // error
    case _    => println("fail")
