object Test {
  def test(x: quoted.Expr[Int])(using scala.quoted.Quotes) = x match {
    case '{ val `$y`: Int = 2; 1 } => // error
      y // error: Not found: y
    case '{ ((`$y`: Int) => 3); 2 } => // error
      y // error: Not found: y
    case '{ def `$f`: Int = 8; 2 } => // error
      f // error: Not found: f
    case _ =>
  }
}
