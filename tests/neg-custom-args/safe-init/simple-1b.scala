class Box(x: Int) {
  val y = x + 1
  val z = f(5)       // error

  List(3, 4, 5).map(_ * 2)

  var a = "hello"

  def f(m: Int) = m + a.size  // error
}

class Boite(x: Int) {
  val y = x + 1
  val z = f(5)       // error

  List(3, 4, 5).map(_ * 2)

  var a = "hello"

  final def f(m: Int) = m + a.size   // error
}