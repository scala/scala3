object Test {
  def test(using s: quoted.Scope)(x: s.Expr[Int]) = x match {
    case '{ (using s: quoted.Scope) => { val a = 4; '{ a }; $y } } => y // error // error: access to value a from wrong staging level
    case _ =>
  }
}
