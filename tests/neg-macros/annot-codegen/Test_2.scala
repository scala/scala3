// scalac: -Yretain-trees

@data class Bar(val one: String, val two: Int, val three: Int, val four: Double): // error: additional methods needed
  // This definition is OK and will be left as is
  def withOne(one: String): Bar =
    data.generated[Bar]()

  def withTwo(two: Int): Bar =
    new Bar("", two, 1, 1.0) // error: body needs to be replaced
