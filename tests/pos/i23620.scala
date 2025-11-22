trait Foo
trait Bar

type FooOrBar = FooOrBar.Type
object FooOrBar:
  opaque type Type <: (Foo | Bar) = Foo | Bar

  def bar: FooOrBar = new Bar {}

trait Buz

@main def main =
  val p: FooOrBar | Buz = FooOrBar.bar

  p match
    case _: Foo => println("foo")
    case _: Buz => println("buz")
    case _: Bar => println("bar")
