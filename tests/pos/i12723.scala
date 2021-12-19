class Fun[|*|[_, _]] {
  enum -->[A, B] {
    case BiId[X, Y]() extends ((X |*| Y) --> (X |*| Y))
  }

  def go[A, B](f: A --> B): Unit =
    f match {
      case -->.BiId() => ()
    }
}
