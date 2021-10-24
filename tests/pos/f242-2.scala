object Test {
  object Unapply {
    def unapply[T1, T2, T3](x: Int)(using T1)(using T2, T3): Option[(T1, T2, T3)] = ???
  }
  given Int = 6
  given String = "s"
  given Boolean = false

  5 match {
    case Unapply(f: Int, _: String, _: Boolean) if f == 5 => ???
  }
}
