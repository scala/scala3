class B {
  class X
}
class A extends B

object Test {
  def foo(a: A#X): Unit = {
    return;
  }
  def test(): Unit = {
    foo(??? : B#X);
  }
}
