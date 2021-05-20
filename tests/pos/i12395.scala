@main def main : Unit =
  val x = 1

  val y = x match
    case 1 => 1
    case _ =>
      println("bad")
      ???
  println(x)