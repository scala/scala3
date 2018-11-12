object Test {
  val f: (Int |=> Char) |=> Boolean = ???
  implicit val n: Int = 3
  implicit val g: Int |=> Char = ???

  f : Boolean
}

object Test2 {
  val f: (Int |=> Char) |=> Boolean = ???
  implicit val s: String = null
  implicit val g: Int |=> String |=> Char = ???

  f : Boolean
}

object Test3 {
  val f: (Int |=> String |=> Char) |=> Boolean = ???
  implicit val n: Int = 3
  implicit val g: Int |=> Char = ???

  f : Boolean
}
