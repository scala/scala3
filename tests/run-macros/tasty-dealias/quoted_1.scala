import scala.quoted._
import scala.tasty._

object Macros {

  inline def dealias[T]: String = ${ impl('[T]) }

  def impl[T](x: quoted.Type[T])(implicit reflect: Reflection): Expr[String] = {
    import reflect._
    x.unseal.tpe.dealias.show.toExpr
  }
}
