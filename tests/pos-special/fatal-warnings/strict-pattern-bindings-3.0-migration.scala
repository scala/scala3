// These tests should pass under -Xfatal-warnings with source version less than 3.2
import language.`3.0-migration`

object Test:
  // from filtering-fors.scala
  val xs: List[AnyRef] = ???

  for ((x: String) <- xs) do ()
  for (y@ (x: String) <- xs) do ()
  for ((x, y) <- xs) do ()

  for ((x: String) <- xs if x.isEmpty) do ()
  for ((x: String) <- xs; y = x) do ()
  for ((x: String) <- xs; (y, z) <- xs) do ()
  for (case (x: String) <- xs; (y, z) <- xs) do ()
  for ((x: String) <- xs; case (y, z) <- xs) do ()

  val pairs: List[AnyRef] = List((1, 2), "hello", (3, 4))
  for ((x, y) <- pairs) yield (y, x)

  // from unchecked-patterns.scala
  val y :: ys = List(1, 2, 3)
  val (1, c) = (1, 2)
  val 1 *: cs = 1 *: Tuple()

  val (_: Int | _: AnyRef) = ??? : AnyRef

  val 1 = 2

  object Positive { def unapply(i: Int): Option[Int] = Some(i).filter(_ > 0) }
  object Always1 { def unapply(i: Int): Some[Int] = Some(i) }
  object Pair { def unapply(t: (Int, Int)): t.type = t }
  object Triple { def unapply(t: (Int, Int, Int)): (Int, Int, Int) = t }

  val Positive(p) = 5
  val Some(s1) = Option(1)
