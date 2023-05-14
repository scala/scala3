class Foo:
  self =>
  type T = this.type
  val foo: T = ???
  object bar:
    inline def baz(): Any =
      ??? : T

  bar.baz()

class Foo2:
  self =>
  object bar:
    inline def baz(): Any = ??? : self.type

  bar.baz()

class Foo3:

  type T
  object bar:
    inline def baz(): Any = ??? : List[T]

  bar.baz()

class Foo4:
  self =>

  type T
  object bar:
    inline def baz(): Any =
      val xs: Foo4 { type T = self.T } = ???
      xs

  bar.baz()
