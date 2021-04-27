class Foo(private val name: String)

extension (f: Foo) def name() = "bar"

@main def Test =
  assert(Foo("foo").name() == "bar")
