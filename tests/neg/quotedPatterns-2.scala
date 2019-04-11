object Test {
  def test(x: quoted.Expr[Int]) given tasty.Reflection = x match {
    case '{ val a = 4; '{ a }; $y } => y // error: access to value a from wrong staging level
    case _ =>
  }
}
