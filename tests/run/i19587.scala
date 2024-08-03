case class Foo(bar: String, baz: Int)
object Foo:
  def withDefaults(bar: String = "", baz: Int = 42) = Foo(bar, baz)

object Test1:
  export Foo.withDefaults

object Test2:
  export Foo.withDefaults as fooWithDefaults

@main def Test =
  // this works
  assert:
    Test1.withDefaults("test1") == Foo("test1", 42)

  // this doesn't work
  assert:
    Test2.fooWithDefaults("test2") == Foo("test2", 42)
