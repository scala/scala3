
import scala.quoted._
class Foo {
  def mcrImpl(using s: Scope)(body: s.Expr[Any])(using t: s.Type[_ <: Any]): s.Expr[Any] = '{ // error
    val tmp = ???.asInstanceOf[$t]
    tmp
  }
}
