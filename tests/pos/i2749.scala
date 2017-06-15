object Test {
  val f: implicit (implicit Int => Char) => Boolean = ???
  implicit val n: Int = 3
  implicit val g: implicit Int => Char = ???

  f : Boolean
}
