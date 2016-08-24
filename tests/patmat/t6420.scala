object Test {
  val c0 = false
  val c1 = true

  def foo(x: List[Boolean], y: List[Boolean]) = (x,y) match {
    case (`c0`::x, `c0`::y) => x
    case (`c0`::x, `c1`::y) => y
    case (`c1`::x, `c0`::y) => y
    case (`c1`::x, `c1`::y) => x
  }
}
