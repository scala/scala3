object Test {
  def foo(a: Boolean, b: Boolean): Unit =
    (a, b) match {
      case (false, _) =>
      case (true,  _) =>
      case (_, false) => // error: unreachable
    }

  def bar(a: Option[Boolean], b: Boolean): Unit =
    (a, b) match {
      case (Some(false), _) =>
      case (Some(true), _) =>
      case (None, _) =>
      case (_, false) => // reachable: null
    }
}
