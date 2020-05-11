import scala.quoted._
import scala.deriving._

case class Foo(i: Int)
case class Box[A](x: A)

object Macro {
  inline def foo[T]: String =
    ${ fooImpl[T] }

  def fooImpl[T](using s: Scope)(implicit t: s.Type[T]): s.Expr[String] = {
    val sym = t.symbol
    if sym.isClassDef then '{ "symbol" }
    else if sym.isNoSymbol then '{ "no symbol" }
    else  '{ "match error" }
  }
}
