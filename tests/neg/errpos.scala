object Test {
  val x =     // error: expression expected
  val y = 2

  val z =     // error: expression expected

  // ...
  val a = 3

  val b = type // error: expression expected (on "type")

  1 match {
    case            // error: pattern expected
    case 2 => ""
  }
}
