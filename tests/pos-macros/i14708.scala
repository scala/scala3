import scala.quoted.*

object Main {
  def foo(a: Expr[Any])(using Quotes) = {
    a match {
      case '{ ($x: Set[t]).toSet } =>
      case _ =>
    }
  }
}
