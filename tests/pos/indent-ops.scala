def test(b: Boolean, y: Int) =
  val first = y
    * y

  val result =
    y > 0
    ||
      val z = y + 1
      z > 0
    ||
      val bb = !b
      bb & b
    ||
      y
      * y
      *
        val z = y * y
        z
      <
        y
