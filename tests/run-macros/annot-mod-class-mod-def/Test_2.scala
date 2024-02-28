//> using options -experimental -Yno-experimental

@modToString("This is Foo")
class Foo:
  override def toString(): String = "?" //> override def toString(): String = "This is Foo"

@modToString("This is Foo object")
object Foo:
  override def toString(): String = "?" //> override def toString(): String = "This is Foo object"

@main def Test(): Unit =
  val foo = new Foo
  assert(foo.toString() == "This is Foo", foo)
  assert(Foo.toString() == "This is Foo object", Foo)
