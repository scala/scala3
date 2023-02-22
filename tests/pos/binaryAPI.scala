package foo

import scala.annotation.binaryAPI

class Foo(@binaryAPI private[Foo] val param: Int, @binaryAPI private[Foo] var param2: Int):
  @binaryAPI
  protected val protectedVal: Int = 2
  @binaryAPI
  private[foo] val packagePrivateVal: Int = 2
  @binaryAPI
  protected var protectedVar: Int = 2
  @binaryAPI
  private[foo] var packagePrivateVar: Int = 2

  inline def foo: Int =
    protectedVar = 3
    packagePrivateVar = 3
    param + param2 + protectedVal + packagePrivateVal + protectedVar + packagePrivateVar

class Bar() extends Foo(3, 3):
  override protected val protectedVal: Int = 2

  override private[foo] val packagePrivateVal: Int = 2

  inline def bar: Int = protectedVal + packagePrivateVal

class Baz() extends Foo(4, 4):
  @binaryAPI // TODO warn? Not needed because Foo.protectedVal is already @binaryAPI
  override protected val protectedVal: Int = 2

  @binaryAPI
  override private[foo] val packagePrivateVal: Int = 2

  inline def baz: Int = protectedVal + packagePrivateVal


class Qux() extends Foo(5, 5):
  inline def qux: Int = protectedVal + packagePrivateVal

def test =
  Foo(3, 3).foo
  Bar().bar
  Baz().baz
  Qux().qux

@binaryAPI given Int = 1
@binaryAPI given (using Double): Int = 1

trait A[T]:
  def f: T
@binaryAPI given A[Int] with
  def f: Int = 1
@binaryAPI given (using Double): A[Int] with
  def f: Int = 1

package inlines {
  // Case that needed to be converted with MakeInlineablePassing
  class C[T](x: T) {
    @binaryAPI private[inlines] def next[U](y: U): (T, U) = (x, y)
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

package foo {
  private object Foo:
    @binaryAPI private[foo] def x: Int = 1
  inline def f: Int = Foo.x
}
def testFoo = foo.f

def localTest =
  class Foo:
    @annotation.binaryAPI private[Foo] val a: Int = 1
    @annotation.binaryAPI protected val b: Int = 1
