//> using options -experimental -Yno-experimental

class Foo:
  val val1: String = "?"
  def def1: String = "?"

@overrideField("val1", "a")
@overrideField("def1", "b")
class Bar extends Foo
  //> val val1: String = "a"
  //> def def1: String = "b"

@overrideField("val1", "a")
@overrideField("def1", "b")
object Foo extends Foo
  //> val val1: String = "a"
  //> def def1: String = "b"

@main def Test(): Unit =
  val foo = new Bar
  assert(foo.val1 == "a", foo.val1)
  assert(foo.def1 == "b", foo.def1)

  assert(Foo.val1 == "a", Foo.val1)
  assert(Foo.def1 == "b", Foo.def1)
