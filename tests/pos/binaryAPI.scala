package foo

import scala.annotation.{binaryAPI, binaryAPIAccessor}

class Foo(@binaryAPIAccessor param: Int, @binaryAPI private[Foo] val paramVal: Int, @binaryAPI private[Foo] var paramVar: Int):
  @binaryAPIAccessor
  private val privateVal: Int = 2
  @binaryAPI
  protected val protectedVal: Int = 2
  @binaryAPI
  private[foo] val packagePrivateVal: Int = 2
  @binaryAPIAccessor
  private val privateVar: Int = 2
  @binaryAPI
  protected var protectedVar: Int = 2
  @binaryAPI
  private[foo] var packagePrivateVar: Int = 2

  inline def foo: Int =
    paramVar = 3
    protectedVar = 3
    packagePrivateVar = 3
    param + paramVal + paramVar + privateVal + protectedVal + packagePrivateVal + protectedVar + packagePrivateVar

class Bar() extends Foo(3, 3, 3):
  override protected val protectedVal: Int = 2

  override private[foo] val packagePrivateVal: Int = 2

  inline def bar: Int = protectedVal + packagePrivateVal

class Baz() extends Foo(4, 4, 4):
  @binaryAPI // TODO warn? Not needed because Foo.protectedVal is already @binaryAPI
  override protected val protectedVal: Int = 2

  @binaryAPI
  override private[foo] val packagePrivateVal: Int = 2

  inline def baz: Int = protectedVal + packagePrivateVal


class Qux() extends Foo(5, 5, 5):
  inline def qux: Int = protectedVal + packagePrivateVal

def test =
  Foo(3, 3, 3).foo
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
    @binaryAPI private[Foo] val a: Int = 1
    @binaryAPI protected val b: Int = 1

package traits {
  trait Trait:
    @binaryAPIAccessor private val myVal = 1
    @binaryAPIAccessor private lazy val myLazyVl = 2
    @binaryAPIAccessor private var myVar = 2
    @binaryAPIAccessor private def myDef = 3
    @binaryAPIAccessor private given myGiven: Int = 4

    @binaryAPI protected val myVal2 = 1
    @binaryAPI protected lazy val myLazyVl2 = 2
    @binaryAPI protected var myVar2 = 2
    @binaryAPI protected def myDef2 = 3
    @binaryAPI protected given myGiven2: Int = 4

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
    @binaryAPIAccessor private def bar: Any = ???
  end Foo

  def test =
    Baz.foo
    (new Baz).foo
    val baz = new Baz
    baz.foo
}

package constructors {
  class Foo @binaryAPI private[constructors] (x: Int):
    @binaryAPI private[constructors] def this(x: Int, y: Int) = this(x + y)

  class Bar @binaryAPI(x: Int):
    @binaryAPI private def this(x: Int, y: Int) = this(x + y)
    inline def bar: Bar = new Bar(x, x)

  inline def newFoo(x: Int) = new Foo(x)
  inline def newFoo(x: Int, y: Int) = new Foo(x, y)
}

def testConstructors =
  val f = constructors.newFoo(1)
  val g = constructors.newFoo(1, 2)
  val h = new constructors.Bar(1).bar
