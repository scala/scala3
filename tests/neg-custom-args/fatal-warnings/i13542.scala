import scala.language.implicitConversions

case class Foo(i: Int) extends AnyVal:
  def toFoo = this

case class Bar(i: Int) extends AnyVal

class BarOps(bar: Bar):
  def toFoo = Foo(bar.i)

implicit def augmentBar(bar: Bar): BarOps = BarOps(bar)

def lazyIdentity[T](x: => T): T = x
def repIdentity[T](x: T*): T = x(0)

val x1 =
  implicit def barToFoo(bar: Bar): Foo = bar.toFoo // error: infinite loop in function body
  val foo: Foo = Bar(1)

val x2 =
  implicit def barToFoo2(bar: Bar): Foo =
    identity(bar.toFoo)  // error
  val foo: Foo = Bar(1)

val x3 =
  implicit def barToFoo3(bar: Bar): Foo =
    lazyIdentity(bar.toFoo)  // OK
  val foo: Foo = Bar(1)

val x4 =
  implicit def barToFoo4(bar: Bar): Foo =
    repIdentity(bar.toFoo)  // error
  val foo: Foo = Bar(1)

val x5 =
  implicit def barToFoo4(bar: Bar): Foo =
    val y = bar.toFoo  // error
    y
  val foo: Foo = Bar(1)

val x6 =
  implicit def barToFoo4(bar: Bar): Foo =
    lazy val y = bar.toFoo  // error
    if false then y else ???
  val foo: Foo = Bar(1)





