import scala.quoted._
class Test {
  def foo(str: Expr[String]) with QuoteContext = '{
    @deprecated($str, "")
    def bar = ???
  }
}
