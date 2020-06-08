
object Test {

  val f2: Tuple2[Int, String] => Int = (x, y) => x
  val g2: Tuple2[Int, String] => Int = _ + _.length

  // FIXME issue #5257
//  val h2: Int *: Int *: EmptyTuple => Int = (x, y) => x + y
//  val k2: Int *: Tuple1[Int] => Int = (x, y) => x + y

  type T2 = Tuple2[Int, Int]
  val h2: T2 => Int = (x1, x2) => 2

  val f3: Tuple3[Int, Int, Int] => Int = (x1, x2, x3) => 3
  val g3: Tuple3[Int, Int, Int] => Int = _ + _ + _

  val f5: Tuple5[Int, Int, Int, Int, Int] => Int = (x1, x2, x3, x4, x5) => 5
  val g5: Tuple5[Int, Int, Int, Int, Int] => Int = _ + _ + _ + _ + _

  val f10: Tuple10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int] => Int =
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) => 10
  val g10: Tuple10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int] => Int =
    _ + _ + _ + _ + _ + _ + _ + _ + _ + _

  val f22: Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int] => Int =
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22) => 22
  val g22: Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int] => Int =
    _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _

  // FIMXE Tuples of size larger that 22 are not supported yet (issue #5256)
//  val f23: ((Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) => Int =
//    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23) => 22
//  val g23: ((Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) => Int =
//    _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _

//  type T23 = (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
//  val h23: T23 => Int = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23) => 23

}
