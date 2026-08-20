object Test {
  object E1 {
    def unapply(x: Int): Some[Tuple.Map[(Int, Int), Option]] = ???
  }

  val s: Int = ???
  s match {
    case E1(x, y) => x
  }
}
