class Foo with
  extension(x: String) def run: Unit  = ???

object Bar extends Foo with
  def run(v: Int): Unit = ???

  "ABC".run  // Failed before:  Reference to run is ambiguous...
