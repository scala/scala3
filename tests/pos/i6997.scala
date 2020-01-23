
import scala.quoted._
class Foo {
  def mcrImpl(body: Expr[Any]) with (t: Type[_ <: Any]) with (ctx: QuoteContext) : Expr[Any] = '{
    val tmp = ???.asInstanceOf[$t]
    tmp
  }
}
