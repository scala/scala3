import scala.quoted._
object Test {
  def test(x: quoted.Expr[Int])(using QuoteContext) = x match {
    case '{ poly[${Foo(t)}]($x); 4 } => ??? // error
    case '{ type $t; poly[${Foo(y: quoted.Staged[`$t`])}]($x); 4 } => ??? // error
    case _ =>
  }

  def poly[T](x: T): Unit = ()

}

