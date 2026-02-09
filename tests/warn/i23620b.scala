trait Foo
trait Bar

type FooOrBar = FooOrBar.Type
object FooOrBar:
  opaque type Type <: (Foo | Bar) = Foo | Bar

  def bar: FooOrBar = new Bar {}

type OnlyFoo = OnlyFoo.Type
object OnlyFoo:
  opaque type Type <: (Foo | Bar) = Foo

  def foo: OnlyFoo = new Foo {}

@main def main =
  val p: FooOrBar= FooOrBar.bar
  val p2: OnlyFoo = OnlyFoo.foo

  p match // warn
    case _: Foo => println("foo")

  p2 match { // warn
    case _: Foo => println("foo")
  }

sealed trait S
trait Z

case object A extends S, Z
case object B extends S, Z

trait HasT:
  type T <: S & Z

def nonExhaustive(h: HasT, x: h.T) =
  x match // warn
    case A => ()
