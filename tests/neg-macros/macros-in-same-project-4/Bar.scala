// nopos-error
import scala.quoted._

object Bar {

  Foo.myMacro()

  def hello()(using Quotes): Expr[Unit] = '{ println("Hello") }
}
