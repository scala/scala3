
import scala.quoted._
class Foo {
  def mcrImpl(body: Expr[Any])(using t: Staged[_ <: Any])(using ctx: QuoteContext): Expr[Any] = '{
    val tmp = ???.asInstanceOf[$t]
    tmp
  }
}
