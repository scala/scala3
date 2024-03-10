//> using options -experimental -Yno-experimental

@genToString("This is Foo")
class Foo
  //> override def toString(): String = "This is Foo"

@genToString("This is Foo object")
object Foo
  //> override def toString(): String = "This is Foo"

@main def Test(): Unit =
  val foo = new Foo
  assert(foo.toString() == "This is Foo", foo)
  assert(Foo.toString() == "This is Foo object", Foo)
