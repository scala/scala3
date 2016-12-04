object Test {
  def main(args: Array[String]) = {

    implicit val world: String = "world!"

    val i1 = (implicit (s: String) => s.length > 2)
    val i2 = {implicit (s: String) => s.length > 2}

    assert(i1)
    assert(i2)

    val x: implicit String => Boolean = { implicit (s: String) => s.length > 2 }

    val xx: implicit (String, Int) => Int = implicit (x: String, y: Int) => x.length + y

    val y: String => Boolean = x

    val yy: (String, Int) => Any = xx

    val b = x("hello")

    val b1: Boolean = b

    val bi = x

    val bi1: Boolean = bi

    val c = xx("hh", 22)

    val c1: Int = c
  }
}
