case class Foo()

object Test {
  def main(args: Array[String]): Unit = {
    assert(Foo.unapply(Foo()) == true)

    // unapply generate by scalac are `_ != null`,
    // dotty returns true in all cases
    assert(Foo.unapply(null) == true)

    Foo() match {
      case Foo() => ()
      case _     => ???
    }

    Foo() match {
      case _: Foo => ()
      case _      => ???
    }

    (Foo(): Any) match {
      case Foo() => ()
      case _     => ???
    }
  }
}
