class Bar:
  private opaque type Baz = Int // error

  private object Foo:
    opaque type O = Int // OK

  val x: Baz = 1

