import scala.quoted.*
class Test {
  def foo(str: Expr[String])(using Quotes) = '{
    @deprecated($str, "")
    def bar = ???
  }
}
