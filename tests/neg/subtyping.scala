class B {
  class X
}
class A extends B

object Test {
  def test(): Unit = {
    implicitly[B#X <:< A#X]
  }
}
