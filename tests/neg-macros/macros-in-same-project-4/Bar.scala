// nopos-error
import scala.quoted.*

object Bar {

  Foo.myMacro()

  def hello()(using Quotes): Expr[Unit] = '{ println("Hello") }
}
