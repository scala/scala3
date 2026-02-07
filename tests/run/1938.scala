case class Large(
  e1: Int,
  e2: Int,
  e3: Int,
  e4: Int,
  e5: Int,
  e6: Int,
  e7: Int,
  e8: Int,
  e9: Int,
  e10: Int,
  e11: Int,
  e12: Int,
  e13: Int,
  e14: Int,
  e15: Int,
  e16: Int,
  e17: Int,
  e18: Int,
  e19: Int,
  e20: Int,
  e21: Int,
  e22: Int,
  e23: Int
)

object Test {
  def main(args: Array[String]): Unit = {
    val l = Large(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)

    assert(l.productArity == 23)

    assert(l.productElement(0) == 1)
    assert(l.productElement(1) == 2)
    assert(l.productElement(21) == 22)
    assert(l.productElement(22) == 23)

    try {
      l.productElement(23)
      ???
    } catch {
      case e: IndexOutOfBoundsException => assert(e.getMessage.contains("23"))
      case _ => assert(false)
    }
  }
}
