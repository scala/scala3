object Test extends App {
  val x: Int @unchecked = 2
  val (y1: Some[Int] @unchecked) = Some(1): Option[Int]

  val a :: as: @unchecked = List(1, 2, 3)


}