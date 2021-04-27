import scala.quoted.*
import scala.deriving.*

case class Foo(i: Int)
case class Box[A](x: A)

object Macro {
  inline def foo[T]: String =
    ${ fooImpl[T] }

  def fooImpl[T](implicit t: Type[T], qctx: Quotes): Expr[String] = {
    import quotes.reflect.*
    val sym = TypeTree.of[T].symbol
    if sym.isClassDef then '{ "symbol" }
    else if sym.isNoSymbol then '{ "no symbol" }
    else  '{ "match error" }
  }
}
