// nopos-error
import scala.quoted._

object Bar {

  Foo.myMacro()

  def hello()(given QuoteContext): Expr[Unit] = '{ println("Hello") }
}
