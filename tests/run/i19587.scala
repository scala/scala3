case class Foo(bar: String, baz: Int)
object Foo:
  def withDefaults(bar: String = "", baz: Int = 42) = Foo(bar, baz)

object Test1:
  export Foo.withDefaults

object Test2:
  export Foo.withDefaults as fooWithDefaults

class Bar:
  infix def bar(other: Bar) = 42

object Baz:
  val b = Bar()
  export b.bar
  export b.bar as baz

@main def Test =
  // this works
  assert:
    Test1.withDefaults("test1") == Foo("test1", 42)

  // this doesn't work
  assert:
    Test2.fooWithDefaults("test2") == Foo("test2", 42)

  val b = Bar()
  println:
    b bar Bar()
  println:
    Baz bar Bar()
  println:
    Baz baz Bar()
