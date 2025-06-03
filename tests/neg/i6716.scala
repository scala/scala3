class Foo

object Bar {
  given Foo()
  given List[Foo] = List(summon[Foo]) // ok
}

object Baz {
  @annotation.nowarn
  given List[Foo] = List(summon[Foo]) // error
  given Foo()
}
