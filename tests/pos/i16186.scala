//> using options -Werror

class Test:
  val x = 42
  val tup23 = (x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x)

  tup23 match {
    case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => "Tuple Pattern"
  }
