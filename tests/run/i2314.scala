case class A(i: Int, s: String)

case class B(i: Int, s: String) {
  // No override, these methods will be added by SyntheticMethods only if
  // there are not user defined.
  def productArity = -1
  def productElement(i: Int): Any = None
}

object Test {
  def main(args: Array[String]): Unit = {
    val a = A(1, "s")
    assert(a.productArity == 2)
    assert(a.productElement(0) == 1)
    assert(a.productElement(1) == "s")

    try {
      a.productElement(-1)
      ???
    } catch {
      case e: IndexOutOfBoundsException => assert(e.getMessage().contains("-1"))
      case _ => assert(false)
    }
    try {
      a.productElement(2)
      ???
    } catch {
      case e: IndexOutOfBoundsException => assert(e.getMessage().contains("2"))
      case _ => assert(false)
    }

    val b = B(1, "s")
    assert(b.productArity == -1)
    assert(b.productElement(0) == None)
    assert(b.productElement(1) == None)
  }
}
