
// These tests should fail under -Xfatal-warnings with source version source version 3.2 or later
import language.`3.2`

object Test:
  // from filtering-fors.scala
  val xs: List[AnyRef] = ???

  for ((x: String) <- xs) do ()    // warn
  for (y@ (x: String) <- xs) do () // warn
  for ((x, y) <- xs) do ()         // warn

  for ((x: String) <- xs if x.isEmpty) do ()   // warn
  for ((x: String) <- xs; y = x) do ()          // warn
  for ((x: String) <- xs; (y, z) <- xs) do ()   // warn // warn
  for (case (x: String) <- xs; (y, z) <- xs) do () // warn
  for ((x: String) <- xs; case (y, z) <- xs) do () // warn

  val pairs: List[AnyRef] = List((1, 2), "hello", (3, 4))
  for ((x, y) <- pairs) yield (y, x) // warn

  // from unchecked-patterns.scala
  val y :: ys = List(1, 2, 3)        // warn
  val (1, c) = (1, 2)                // warn
  val 1 *: cs = 1 *: Tuple()         // warn

  val (_: Int | _: AnyRef) = ??? : AnyRef  // warn

  val 1 = 2  // warn

  object Positive { def unapply(i: Int): Option[Int] = Some(i).filter(_ > 0) }
  object Always1 { def unapply(i: Int): Some[Int] = Some(i) }
  object Pair { def unapply(t: (Int, Int)): t.type = t }
  object Triple { def unapply(t: (Int, Int, Int)): (Int, Int, Int) = t }

  val Positive(p) = 5                // warn
  val Some(s1) = Option(1)           // warn