object Test {
  (1, 2) match {
    case (1, x, 3) => println(x) // error: unreachable
  }
  "A" match {
    case (1, 2, y) => println(y) // error: unreachable
  }
}
