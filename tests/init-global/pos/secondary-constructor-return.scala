class Foo (var x: Int) {
  def this(a : Int, b : Int) = {
    this(a + b)
    return
  }
  val y = x
}

object A {
  val a = new Foo(2, 3)
  val b = a.y
}