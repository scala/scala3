// nopos-error
import scala.quoted._

object Bar {

  Foo.myMacro()

  def hello(using s: Scope)(): s.Expr[Unit] = '{ println("Hello") }
}
