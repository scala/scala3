class Foo:
  extension(x: String) def run: Unit  = ???

object Bar extends Foo:
  def run(v: Int): Unit = ???

  "ABC".run  // Failed before:  Reference to run is ambiguous...
