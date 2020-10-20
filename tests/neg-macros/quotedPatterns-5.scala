import scala.quoted._
object Test {
  def test(x: quoted.Expr[Int])(using QuoteContext): Unit = x match {
    case '{ type $T; 4 } => Type[T]
    case '{ type $T; poly[$T]($x); 4 } => // error: duplicate pattern variable: T
    case '{ type `$T`; poly[`$T`]($x); 4 } =>
      Type[T] // error
    case _ =>
  }

  def poly[T](x: T): Unit = ()

}
