object Test {
  def test(using s: quoted.Scope)(x: s.Expr[Int]) = x match {
    case '{ val `$y`: Int = 2; 1 } => // error
      y // error: Not found: y
    case '{ ((`$y`: Int) => 3); 2 } => // error
      y // error: Not found: y
    case '{ def `$f`: Int = 8; 2 } => // error
      f // error: Not found: f
    case _ =>
  }
}
