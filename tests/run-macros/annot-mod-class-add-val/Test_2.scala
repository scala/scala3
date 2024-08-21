//> using options -experimental

@addMemoToString("This is Foo")
class Foo
  //> private val string$macro$1: String = "This is Foo"
  //> def toString(): String = string$macro$1

@addMemoToString("This is Foo object")
object Foo
  //> private val string$macro$2: String = "This is Foo object"
  //> def toString(): String = string$macro$2

@main def Test(): Unit =
  val foo = new Foo
  assert(foo.toString() == "This is Foo", foo)
  assert(Foo.toString() == "This is Foo object", Foo)
