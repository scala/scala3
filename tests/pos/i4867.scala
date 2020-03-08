object UnionMapping {
  private def parse(string: String): Int | Double = {
    if(string.contains("."))
      string.toDouble
    else
      string.toInt
  }

  def test_number = {
    val strings: Seq[String] = Seq("123", "2.0", "42")
    // Works
    val asdf: Seq[AnyVal] = strings.map(parse(_))
    // Fails to compile
    val union: Seq[Int | Double] = strings.map(parse(_))
  }
}