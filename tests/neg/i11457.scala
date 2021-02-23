
val x = Seq(1, 2) match
    case Seq(x, y*) => println(y) // prints List(2) which looks correct

val y = Seq(1, 2) match
  case Seq(x, (y)*) => println(y) // error
