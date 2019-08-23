
import scala.quoted._
class Foo {
  def mcrImpl(body: Expr[Any]) given (t: Type[_ <: Any]) given (ctx: QuoteContext): Expr[Any] = '{
    val tmp = ???.asInstanceOf[$t]
    tmp
  }
}
