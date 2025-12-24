class Foo[T] {
  def m(x: T): Boolean = true
  def m(x: T, y: Int): Boolean = false
}

def test(): Unit = {
  val x: Foo[Float] = new Foo[Float]()
  val y: Function1[? >: Float, Boolean] = x.m _
}
