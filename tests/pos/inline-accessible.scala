// package foo

import scala.annotation.inlineAccessible

class Foo(@inlineAccessible param: Int):
  @inlineAccessible
  private val privateVal: Int = 2

  @inlineAccessible
  protected val protectedVal: Int = 2

  @inlineAccessible
  private[foo] def packagePrivateVal: Int = 2
  inline def foo: Int = param + privateVal + protectedVal + packagePrivateVal

class Bar() extends Foo(3):
  override protected val protectedVal: Int = 2

  override private[foo] def packagePrivateVal: Int = 2

  inline def bar: Int = protectedVal + packagePrivateVal // TODO reuse accessor in Foo

class Baz() extends Foo(4):
  @inlineAccessible // TODO warn? : does not need an accessor and it overrides a field that has an accessor.
  override protected val protectedVal: Int = 2

  @inlineAccessible
  override private[foo] def packagePrivateVal: Int = 2

  inline def baz: Int = protectedVal + packagePrivateVal


class Qux() extends Foo(5):
  inline def qux: Int = protectedVal + packagePrivateVal

def test =
  @inlineAccessible // noop
  val a = 5

  Foo(3).foo
  Bar().bar
  Baz().baz
  Qux().qux


package inlines {
  // Case that needed to be converted with MakeInlineablePassing
  class C[T](x: T) {
    @inlineAccessible private[inlines] def next[U](y: U): (T, U) = (x, y)
  }
  class TestPassing {
    inline def foo[A](x: A): (A, Int) = {
      val c = new C[A](x)
      c.next(1)
    }
    inline def bar[A](x: A): (A, String) = {
      val c = new C[A](x)
      c.next("")
    }
  }
}
