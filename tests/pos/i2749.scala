object Test {
  val f: implicit (implicit Int => Char) => Boolean = ???
  implicit val n: Int = 3
  implicit val g: implicit Int => Char = ???

  f : Boolean
}

object Test2 {
  val f: implicit (implicit Int => Char) => Boolean = ???
  implicit val s: String = null
  implicit val g: implicit Int => implicit String => Char = ???

  f : Boolean
}

object Test3 {
  val f: implicit (implicit Int => implicit String => Char) => Boolean = ???
  implicit val n: Int = 3
  implicit val g: implicit Int => Char = ???

  f : Boolean
}
