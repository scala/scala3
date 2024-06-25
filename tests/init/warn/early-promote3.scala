abstract class A {
  bar()
  private val a = 5
  def foo() = a
  def bar(): Unit
}

class M extends A {
  def bar() = promote(this)    // warn
  def promote(m: M) = m.foo()
}
