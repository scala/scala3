object Test {
  def test(x: quoted.Expr[Int])(using scala.quoted.Quotes) = x match {
    case '{ val a = '{ println($y) }; 0 } => ??? // error: Not found: y
    case _ =>
  }
}
