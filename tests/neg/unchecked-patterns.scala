object Test {
  val (y1: Some[Int] @unchecked) = Some(1): Option[Int]  // OK
  val y2: Some[Int] @unchecked = Some(1): Option[Int]    // error
}