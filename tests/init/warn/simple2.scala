class Box(x: Int) {
  val y = x + 1
  val z = f(5)

  List(3, 4, 5).map(_ * 2)

  private var a = "hello"    // warn

  def f(m: Int) = m + a.size
}

class Box2(x: Int) {
  var a = "hello"
  val y = x + 1
  val z = f(5)

  List(3, 4, 5).map(_ * 2)

  def f(m: Int) = y + a.size
}
