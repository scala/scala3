object Test {
  def test(x: quoted.Expr[Int]) given tasty.Reflection = x match {
    case '{ type $t; poly[$t]($x); 4 } => ??? // error: duplicate pattern variable: $t
    case '{ type `$t`; poly[`$t`]($x); 4 } =>
      val tt: quoted.Type[_] = t // error
      ???
    case _ =>
  }

  def poly[T](x: T): Unit = ()

}
