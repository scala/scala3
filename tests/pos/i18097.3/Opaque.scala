package test

type Foo = Unit
val bar: Foo = ()

opaque type Opaque = Unit

extension (foo: Foo)
  def go: Option[Opaque] = ???
