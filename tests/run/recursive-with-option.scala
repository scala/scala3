//> using options -opt-inline:**

// Regression test for an optimizer bug originally found while compiling ScalaJS

object Test {
  def foo(b: Option[Int]): Unit =
    fooRec(x = 0, b)

  private def fooRec(x: Int, b: Option[Int]): Unit = {
    if (b.nonEmpty) {
      fooRec(x + 1, b)
    }
  }

  def main(args: Array[String]): Unit = {
    foo(None)
  }
}
