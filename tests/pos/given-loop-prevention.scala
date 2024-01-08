//> using options -Xfatal-warnings

class Foo

object Bar {
  given Foo with {}
  given List[Foo] = List(summon[Foo]) // ok
}

object Baz {
  @annotation.nowarn
  given List[Foo] = List(summon[Foo]) // gives a warning, which is suppressed
  given Foo with {}
}
