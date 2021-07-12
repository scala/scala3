object Foo:
  @annotation.implicitNotFound("Oops")
  type Bar

@main def run(): Unit =
  summon[Foo.Bar]     // error