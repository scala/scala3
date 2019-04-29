object Test extends App {
  val x: Int @unchecked = 2
  val (y1: Some[Int] @unchecked) = Some(1): Option[Int]

  val a :: as: @unchecked = List(1, 2, 3)
  val lst @ b :: bs: @unchecked = List(1, 2, 3)
  val (1, c): @unchecked = (1, 2)
  val 1 *: cs: @unchecked = 1 *: ()        // error
}