case class MyClass(v1: Int, v2: Int, v3: Int, v4: Int) extends Product3[Int, Int, Int]:
  val _1: Int = v2
  def _2: Int = v3
  var _3: Int = v4
  def _4(x: Boolean): Int = 0

@main def Test =
  val c = MyClass(1, 2, 3, 4)
  assert(c._1 == 2)
  assert(c._2 == 3)
  assert(c._3 == 4)
  assert(c._4 == 4)
