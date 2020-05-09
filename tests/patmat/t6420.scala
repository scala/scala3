object Test {
  val c0 = false
  val c1 = true

  def foo(x: List[Boolean], y: List[Boolean]) = (x,y) match {
    case (`c0`::x, `c0`::y) => x
    case (`c0`::x, `c1`::y) => y
    case (`c1`::x, `c0`::y) => y
    case (`c1`::x, `c1`::y) => x
  }

  def bar(x: List[Boolean], y: List[Boolean]) = (x,y) match {
    case (false ::x, false ::y) => x
    case (false ::x, true ::y) => y
    case (true ::x, false ::y) => y
    case (true ::x, true ::y) => x
    case (Nil, _) => Nil
    case (_, Nil) => Nil
  }
}
