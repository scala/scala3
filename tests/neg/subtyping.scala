class B {
  class X
}
class A extends B

object Test {
  def test1(): Unit = {
    implicitly[B#X <:< A#X] // error: no implicit argument
  }
  def test2(): Unit = {
    val a : { type T; type U } = ???
    implicitly[a.T <:< a.U] // error: no implicit argument
  }
}
