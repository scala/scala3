package a

object Bar:
  given Foo.Local()
  inline def bar = Foo.foo // `Bar.bar` is inline, it will hash the body of `Foo.foo`
