object Test {
  val x =     // error: expression expected
  val y = 2   // error: ';' expected

  val z =     // error: expression expected

  // ...
  val a = 3   // error: ';' expected

  val b = type // error: expression expected (on "type")

  1 match {
    case            // error: pattern expected // error: cannot compare with Null
    case 2 => ""
  }
}