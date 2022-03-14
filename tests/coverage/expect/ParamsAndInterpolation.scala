package covtest

object ParamsAndInterpolation:

  def simple(a: Int, b: String): String =
    s"$a, ${b.length}"

  def test(): Unit =
    val xs: List[String] = List("d", "o", "t", "t", "y")

    xs.zipWithIndex.map((s, i) => println(s"$i: $s"))
