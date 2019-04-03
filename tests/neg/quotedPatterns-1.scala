object Test {
  def test(x: quoted.Expr[Int]) given tasty.Reflection = x match {
    case '{ val a = '{ println($y) }; 0 } => ??? // error: Not found: y
    case _ =>
  }
}
