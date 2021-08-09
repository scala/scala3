object Test extends App {
  val x23 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
  type T23 = (Int, Int, Int, Int, Int,
              Int, Int, Int, Int, Int,
              Int, Int, Int, Int, Int,
              Int, Int, Int, Int, Int,
              Int, Int, Int)
  val x23c: T23 = x23
  println(x23)
  assert(x23(0) == 1)
  assert(x23(22) == 23)

  x23 match {
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23) =>
      println(x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23)
  }
  /* TODO: re-enable
  inline def decompose3 = inline x23 match { case x *: y *: xs => (x, y, xs) }

  { val (x, y, xs) = decompose3
    val xc: Int = x
    val yc: Int = y
    val xsc: Unit = xs
    println(s"$x23 -> $x, $y, $xs")
  }
  */

  val x23s: 23 = x23.size
}