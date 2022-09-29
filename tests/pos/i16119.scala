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
