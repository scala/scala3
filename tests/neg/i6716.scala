class Foo

object Bar {
  given Foo with {}
  given List[Foo] = List(summon[Foo]) // ok
}

object Baz {
  @annotation.nowarn
  given List[Foo] = List(summon[Foo]) // error
  given Foo with {}
}
