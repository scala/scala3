object Test {
  object Unapply {
    def unapply(x: Int): Option[Any] = ???
  }

  5 match {
    case Unapply(f: Int) if f == 5 => ???
  }
}
