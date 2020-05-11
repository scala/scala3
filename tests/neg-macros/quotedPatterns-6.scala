import scala.quoted._
object Test {
  def test(using s: Scope)(x: s.Expr[Int]) = x match {
    case '{ poly[${Foo(t)}]($x); 4 } => ??? // error
    case '{ type $t; poly[${Foo(y: s.Type[`$t`])}]($x); 4 } => ??? // error
    case _ =>
  }

  def poly[T](x: T): Unit = ()

}

