object Test {
  def test(x: quoted.Expr[Int]) given scala.quoted.QuoteContext = x match {
    case '{ val `$y`: Int = 2; 1 } =>
      y // error: Not found: y
    case '{ ((`$y`: Int) => 3); 2 } =>
      y // error: Not found: y
    case '{ def `$f`: Int = 8; 2 } =>
      f // error: Not found: f
    case _ =>
  }
}
