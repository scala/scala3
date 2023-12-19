//> using options -Werror -WunstableInlineAccessors

package foo

import scala.annotation.publicInBinary

class Foo(@publicInBinary private[Foo] val paramVal: Int, @publicInBinary private[Foo] var paramVar: Int):
  @publicInBinary
  protected val protectedVal: Int = 2
  @publicInBinary
  private[foo] val packagePrivateVal: Int = 2
  @publicInBinary
  protected var protectedVar: Int = 2
  @publicInBinary
  private[foo] var packagePrivateVar: Int = 2

  inline def foo: Int =
    paramVar = 3
    protectedVar = 3
    packagePrivateVar = 3
    paramVal + paramVar + protectedVal + packagePrivateVal + protectedVar + packagePrivateVar

class Bar() extends Foo(3, 3):
  @publicInBinary
  override protected val protectedVal: Int = 2
  @publicInBinary
  override private[foo] val packagePrivateVal: Int = 2

  inline def bar: Int = protectedVal + packagePrivateVal

class Baz() extends Foo(4, 4):
  @publicInBinary
  override protected val protectedVal: Int = 2

  @publicInBinary
  override private[foo] val packagePrivateVal: Int = 2

  inline def baz: Int = protectedVal + packagePrivateVal


class Qux() extends Foo(5, 5):
  inline def qux: Int = protectedVal + packagePrivateVal


@publicInBinary given Int = 1
@publicInBinary given (using Double): Int = 1

trait A[T]:
  def f: T
@publicInBinary given A[Int] with
  def f: Int = 1
@publicInBinary given (using Double): A[Int] with
  def f: Int = 1

package inlines {
  // Case that needed to be converted with MakeInlineablePassing
  class C[T](x: T) {
    @publicInBinary private[inlines] def next[U](y: U): (T, U) = (x, y)
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
  @publicInBinary private object Foo:
    @publicInBinary private[foo] def x: Int = 1
  inline def f: Int = Foo.x
}
def testFoo() = foo.f

def localTest() =
  class Foo:
    @publicInBinary private[Foo] val a: Int = 1
    @publicInBinary protected val b: Int = 1

package traits {
  trait Trait:
    @publicInBinary private[Trait] val myVal = 1
    @publicInBinary private[Trait] lazy val myLazyVl = 2
    @publicInBinary private[Trait] var myVar = 2
    @publicInBinary private[Trait] def myDef = 3
    @publicInBinary private[Trait] given myGiven: Int = 4

    @publicInBinary protected val myVal2 = 1
    @publicInBinary protected lazy val myLazyVl2 = 2
    @publicInBinary protected var myVar2 = 2
    @publicInBinary protected def myDef2 = 3
    @publicInBinary protected given myGiven2: Int = 4

    inline def inlined: Unit =
      myVar2 = 1
      myVar = 1
      myVal + myLazyVl + myVar + myDef + myGiven +
      myVal2 + myLazyVl2 + myVar2 + myDef2 + myGiven2

  def testTrait(t: Trait) = t.inlined

  class Baz extends Foo
  object Baz extends Foo

  trait Foo:
    inline def foo: Any = bar
    @publicInBinary private[Foo] def bar: Any = 2
  end Foo

  def test() =
    Baz.foo
    (new Baz).foo
    val baz = new Baz
    baz.foo
}

package constructors {
  class Foo @publicInBinary private[constructors] (x: Int):
    @publicInBinary private[constructors] def this(x: Int, y: Int) = this(x + y)

  class Bar @publicInBinary(@publicInBinary private[Bar] val x: Int):
    @publicInBinary private def this(x: Int, y: Int) = this(x + y)
    inline def bar: Bar = new Bar(x, x)

  inline def newFoo(x: Int) = new Foo(x)
  inline def newFoo(x: Int, y: Int) = new Foo(x, y)
}

def testConstructors() =
  val f = constructors.newFoo(1)
  val g = constructors.newFoo(1, 2)
  val h = new constructors.Bar(1).bar
