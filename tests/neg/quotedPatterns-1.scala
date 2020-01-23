object Test {
  def test(x: quoted.Expr[Int]) with scala.quoted.QuoteContext = x match {
    case '{ val a = '{ println($y) }; 0 } => ??? // error: Not found: y
    case _ =>
  }
}
