object Bar {
  def unapply(x: String): Some[Int] =
    Some(0)
}

object Test {
  def test(x: String) =
    x match {
      case Bar(a) => a
      case _ => x // this case is reachable, i.e. test(null)
    }
}
