package foo {
  protected[foo] trait A {
    def a: Unit = {}
  }
  class B extends A
}
trait C extends foo.B
object Test {
  def test: Unit = {
    val c = new C {}
    c.a
  }
}
