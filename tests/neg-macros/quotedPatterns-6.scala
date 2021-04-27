import scala.quoted.*
object Test {
  def test(x: quoted.Expr[Int])(using Quotes) = x match {
    case '{ poly[${Foo(t)}]($x); 4 } => ??? // error
    case '{ type t; poly[${Foo(y: Type[`t`])}]($x); 4 } => ??? // error
    case _ =>
  }

  def poly[T](x: T): Unit = ()

}

