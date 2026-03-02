//> using options -experimental

@addIndirectToString("This is Foo")
class Foo
  //> private def string$macro$1: String = "This is Foo"
  //> def toString(): String = string$macro$1

@addIndirectToString("This is Foo object")
object Foo
  //> private def string$macro$2: String = "This is Foo object"
  //> def toString(): String = string$macro$2

@main def Test(): Unit =
  val foo = new Foo
  assert(foo.toString() == "This is Foo", foo)
  assert(Foo.toString() == "This is Foo object", Foo)
