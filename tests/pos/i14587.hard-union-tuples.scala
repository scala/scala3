// scalac: -Werror
class Test:
  type Foo = Option[String] | Option[Int]

  def test(foo: Foo) =
    val (_, foo2: Foo) = ( // was: the type test for Test.this.Foo cannot be checked at runtime
      foo match
        case Some(s: String) => ((), s)
        case _               => ((), 0)
    ): @unchecked
    foo2
