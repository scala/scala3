
case class X2[A, B](a: A, b: B)
case class X3[A, B, C](a: A, b: B, c: C)
case class X4[A, B, C, D](a: A, b: B, c: C, d: D)
case class X5[A, B, C, D, E](a: A, b: B, c: C, d: D, e: E)
case class X6[A, B, C, D, E, F](a: A, b: B, c: C, d: D, e: E, f: F)

object Test {
  def main(args: Array[String]) = {

    val x2 = X2("a", 2)
    val X2(a2, b2) = x2

    assert(a2 == "a")
    assert(b2 == 2)

    val x3 = X3("a", 2, "b")
    val X3(a3, b3, c3) = x3

    assert(a3 == "a")
    assert(b3 == 2)
    assert(c3 == "b")

    val x4 = X4("a", 2, "b", 3)
    val X4(a4, b4, c4, d4) = x4

    assert(a4 == "a")
    assert(b4 == 2)
    assert(c4 == "b")
    assert(d4 == 3)

    val x5 = X5("a", 2, "b", 3, "c")
    val X5(a5, b5, c5, d5, e5) = x5

    assert(a5 == "a")
    assert(b5 == 2)
    assert(c5 == "b")
    assert(d5 == 3)
    assert(e5 == "c")

    val x6 = X6("a", 2, "b", 3, "c", 4)
    val X6(a6, b6, c6, d6, e6, f6) = x6

    assert(a6 == "a")
    assert(b6 == 2)
    assert(c6 == "b")
    assert(d6 == 3)
    assert(e6 == "c")
    assert(f6 == 4)
  }

  def any = {
    val x: Any = null
    val X2(a2, b2) = x
    val X3(a3, b3, c3) = x
    val X4(a4, b4, c4, d4) = x
    val X5(a5, b5, c5, d5, e5) = x
    val X6(a6, b6, c6, d6, e6, f6) = x
  }
}
