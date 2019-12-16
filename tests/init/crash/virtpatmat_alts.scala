/*
 * filter: It would fail on the following input
 */
object Test {
  (true, true) match {              // error
    case (true, true) | (false, false) => 1
  }

  List(5) match {                   // error
    case 1 :: Nil | 2 :: Nil  => println("FAILED")
    case (x@(4 | 5 | 6)) :: Nil => println("OK "+ x)
    case 7 :: Nil  => println("FAILED")
    case Nil  => println("FAILED")
  }
}
