object Test extends App {
  val x: Int @unchecked = 2
  val (y1: Some[Int] @unchecked) = Some(1): Option[Int]

  val a :: as: @unchecked = List(1, 2, 3)
  val lst @ b :: bs: @unchecked = List(1, 2, 3)
  val (1, c): @unchecked = (1, 2)

  object Positive { def unapply(i: Int): Option[Int] = Some(i).filter(_ > 0) }
  val Positive(p): @unchecked = 5
}