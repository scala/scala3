final class Bar:
  inline def baz = Baz

private object Baz

@main def Test: Unit =
  val bar = new Bar
  bar.baz
