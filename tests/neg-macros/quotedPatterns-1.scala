object Test {
  def test(using s: quoted.Scope)(x: s.Expr[Int]) = x match {
    case '{ (using s: quoted.Scope) => { val a = '{ println($y) }; 0 } } => ??? // error: Not found: y
    case _ =>
  }
}
