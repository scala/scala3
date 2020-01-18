// nopos-error
import scala.quoted._

object Bar {

  Foo.myMacro()

  def hello() with QuoteContext : Expr[Unit] = '{ println("Hello") }
}
