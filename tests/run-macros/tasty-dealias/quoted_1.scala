import scala.quoted._

object Macros {

  inline def dealias[T]: String = ${ impl('[T]) }

  def impl[T](using s: Scope)(x: s.Type[T]): s.Expr[String] =
    Expr(x.tpe.dealias.show)

}
