object Test:

  def simple(a: Int, b: String): String =
    s"$a, ${b.length}"

  def hexa(i: Int): String =
    f"0x${i}%04x"

  def main(args: Array[String]): Unit =
    val xs: List[String] = List("d", "o", "t", "t", "y")
    xs.zipWithIndex.map((s, i) => println(s"$i: $s"))

    println(simple(1, "abc"))
    println(hexa(127))
    println(raw"a\nb")
