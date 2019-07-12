import scala.quoted._
import scala.deriving._

case class Foo(i: Int)
case class Box[A](x: A)

object Macro {
  inline def foo[T](implicit inline t: Type[T]): String =
    ${ fooImpl }

  def fooImpl[T](implicit t: Type[T], q: QuoteContext): Expr[String] = {
    import q.tasty._
    t.unseal.symbol match {
      case IsClassDefSymbol(self) => '{ "symbol" }
      case NoSymbol() => '{ "no symbol" }
      case _ => '{ "match error" }
    }
  }
}
