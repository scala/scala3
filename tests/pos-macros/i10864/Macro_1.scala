//> using options -experimental

import scala.quoted._

case class T(t: Type[_])

object T {
  def impl[T <: AnyKind](using tt: Type[T])(using Quotes): Expr[Unit] = {
    val t = T(tt)
    t.t match
      case '[type x <: AnyKind; x] => // ok
      case _ => quotes.reflect.report.error("not ok :(")
    '{}
  }

  inline def run[T <: AnyKind] = ${ impl[T] }
}
