object Test {
  def test(x: quoted.Expr[Int]) given tasty.Reflection = x match {
//    case '{ val a = 4; '{ a }; $y } => y // error: access to value a from wrong staging level
    case '{ val `$y`: Int = 2; 1 } =>
      y // error
    case '{ ((`$y`: Int) => 3); 2 } =>
      y // error
    case _ =>
  }
}
