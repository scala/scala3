import scala.quoted._
class Test {
  def foo(using s: Scope)(str: s.Expr[String]) = '{
    @deprecated($str, "")
    def bar = ???
  }
}
