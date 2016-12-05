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

    object nested {
      implicit val empty: String = ""
      assert(!x)
    }

    val yy: (String, Int) => Any = xx

    val z1: implicit String => Boolean = implicitly[String].length >= 2
    assert(z1)

    type StringlyBool = implicit String => Boolean

    val z2: StringlyBool = implicitly[String].length >= 2
    assert(z2)

    type Stringly[T] = implicit String => T

    val z3: Stringly[Boolean] = implicitly[String].length >= 2
    assert(z3)

    type GenericImplicit[X] = implicit X => Boolean

    val z4: GenericImplicit[String] = implicitly[String].length >= 2
    assert(z4)

    val b = x("hello")

    val b1: Boolean = b

    val bi = x

    val bi1: Boolean = bi

    val c = xx("hh", 22)

    val c1: Int = c
  }
}
