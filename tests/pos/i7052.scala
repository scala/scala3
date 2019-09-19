import scala.quoted.{_, given}
class Test {
  def foo(str: Expr[String])(given QuoteContext) = '{
    @deprecated($str, "")
    def bar = ???
  }
}
