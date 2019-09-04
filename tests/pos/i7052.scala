import scala.quoted._
class Test {
  def foo(str: Expr[String]) given QuoteContext = '{
    @deprecated($str, "")
    def bar = ???
  }
}
