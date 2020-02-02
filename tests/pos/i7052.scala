import scala.quoted._
class Test {
  def foo(str: Expr[String])(using QuoteContext) = '{
    @deprecated($str, "")
    def bar = ???
  }
}
