object Test {

  val x: ImplicitFunction1[String, Boolean] = ???

  val y: String => Boolean = x

  val b = x("hello")

  val b1: Boolean = b

}
object Test2 {

  val x: implicit String => Boolean = ???

  val xx: implicit (String, Int) => Int = ???

  val y: String => Boolean = x

  val yy: (String, Int) => Any = xx

  val b = x("hello")

  val b1: Boolean = b

  val c = xx("hh", 22)

  val c1: Int = c

}
