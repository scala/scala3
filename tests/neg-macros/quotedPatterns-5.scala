import scala.quoted.*
object Test {
  def test(x: quoted.Expr[Int])(using Quotes): Unit = x match {
    case '{ type t; 4 } => Type.of[t]
    case '{ type t; poly[t]($x); 4 } => // error: duplicate pattern variable: t
    case '{ type `t`; poly[`t`]($x); 4 } =>
      Type.of[t] // error
    case _ =>
  }

  def poly[T](x: T): Unit = ()

}
