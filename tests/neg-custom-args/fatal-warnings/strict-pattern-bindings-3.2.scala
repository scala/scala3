// These tests should fail under -Xfatal-warnings with source version source version 3.2 or later
import language.`3.2`

object Test:
  // from filtering-fors.scala
  val xs: List[AnyRef] = ???

  for ((x: String) <- xs) do ()    // error
  for (y@ (x: String) <- xs) do () // error
  for ((x, y) <- xs) do ()         // error

  for ((x: String) <- xs if x.isEmpty) do ()   // error
  for ((x: String) <- xs; y = x) do ()          // error
  for ((x: String) <- xs; (y, z) <- xs) do ()   // error // error
  for (case (x: String) <- xs; (y, z) <- xs) do () // error
  for ((x: String) <- xs; case (y, z) <- xs) do () // error

  val pairs: List[AnyRef] = List((1, 2), "hello", (3, 4))
  for ((x, y) <- pairs) yield (y, x) // error

  // from unchecked-patterns.scala
  val y :: ys = List(1, 2, 3)        // error
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
