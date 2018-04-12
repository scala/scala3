object Bar {
  def unapply(x: Int): Some[Int] =
    Some(0)
}

object Test {
  def test(x: Int) =
    x match {
      case Bar(a) => a
      case _ => x // should be unreachable
    }
}
