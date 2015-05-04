// scenario one: supercalls in traits
abstract class C {
  def foo: Int = 2
  def baz: Int = 2
}

trait T extends C {
  override def foo = super.foo + 1
}


// scenario 2: supercalls in nested classes
class D extends C with T {
  class I {
    val x= D.super.baz
  }
}
