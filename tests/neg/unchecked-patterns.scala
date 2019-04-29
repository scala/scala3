object Test {
  val (y1: Some[Int] @unchecked) = Some(1): Option[Int]  // OK
  val y2: Some[Int] @unchecked = Some(1): Option[Int]    // error

  val x :: xs = List(1, 2, 3)        // error
  val (1, c) = (1, 2)                // error
  val 1 *: cs = 1 *: ()              // error

  val (_: Int | _: Any) = ??? : Any  // error
}