import scala.quoted._
object Test {
  def test(using s: Scope)(x: s.Expr[Int]) = x match {
    case '{ type $t; poly[$t]($x); 4 } => ??? // error: duplicate pattern variable: $t
    case '{ type `$t`; poly[`$t`]($x); 4 } =>
      val tt: s.Type[_] = t // error
      ???
    case _ =>
  }

  def poly[T](x: T): Unit = ()

}
