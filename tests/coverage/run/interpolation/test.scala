object Test:

  def simple(a: Int, b: String): String =
    s"$a, ${b.length}"

  def main(args: Array[String]): Unit =
    val xs: List[String] = List("d", "o", "t", "t", "y")

    xs.zipWithIndex.map((s, i) => println(s"$i: $s"))
