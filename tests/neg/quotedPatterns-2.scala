object Test {
  def test(x: quoted.Expr[Int]) with scala.quoted.QuoteContext = x match {
    case '{ val a = 4; '{ a }; $y } => y // error // error: access to value a from wrong staging level
    case _ =>
  }
}
