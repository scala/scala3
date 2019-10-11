import scala.quoted._
import scala.deriving._

case class Foo(i: Int)
case class Box[A](x: A)

object Macro {
  inline def foo[T](implicit inline t: Type[T]): String =
    ${ fooImpl }

  def fooImpl[T](implicit t: Type[T], qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{_, given}
    val sym = t.unseal.symbol
    if sym.isClassDef then '{ "symbol" }
    else if sym.isNoSymbol then '{ "no symbol" }
    else  '{ "match error" }
  }
}
