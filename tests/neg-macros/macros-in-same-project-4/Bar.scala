// nopos-error
import scala.quoted._

object Bar {

  Foo.myMacro()

  def hello()(using QuoteContext): Expr[Unit] = '{ println("Hello") }
}
