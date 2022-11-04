object Test {

  val (y1: Some[Int]) = Some(1): Option[Int] @unchecked  // OK
  val y2: Some[Int] @unchecked = Some(1): Option[Int]    // error

  val x :: xs = List(1, 2, 3)        // error
  val (1, c) = (1, 2)                // error
  val 1 *: cs = 1 *: Tuple()         // error

  val (_: Int | _: AnyRef) = ??? : AnyRef  // error

  val 1 = 2  // error

  object Positive { def unapply(i: Int): Option[Int] = Some(i).filter(_ > 0) }
  object Always1 { def unapply(i: Int): Some[Int] = Some(i) }
  object Pair { def unapply(t: (Int, Int)): t.type = t }
  object Triple { def unapply(t: (Int, Int, Int)): (Int, Int, Int) = t }

  val Positive(p) = 5                // error
  val Some(s1) = Option(1)           // error
  val Some(s2) = Some(1)             // OK
  val Always1(p1) = 5                // OK
  val Pair(t1, t2) = (5, 5)          // OK
  val Triple(u1, u2, u3) = (5, 5, 5) // OK
}