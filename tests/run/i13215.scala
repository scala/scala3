package foo {
  trait Bar:
    inline def baz = Baz

  private[foo] object Baz
}

@main def Test: Unit =
  val bar = new foo.Bar {}
  bar.baz
