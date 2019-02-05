object Test {
  val f: given (given Int => Char) => Boolean = ???
  implicit val n: Int = 3
  implicit val g: given Int => Char = ???

  f : Boolean
}

object Test2 {
  val f: given (given Int => Char) => Boolean = ???
  implicit val s: String = null
  implicit val g: given Int => given String => Char = ???

  f : Boolean
}

object Test3 {
  val f: given (given Int => given String => Char) => Boolean = ???
  implicit val n: Int = 3
  implicit val g: given Int => Char = ???

  f : Boolean
}
