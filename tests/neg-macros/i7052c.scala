import scala.quoted.*
class Test {
  def foo(str: Expr[String])(using Quotes) = '{
    @deprecated($str, "") // error: expression cannot be used inside an annotation argument
    def bar = ???
  }
}
