import scala.quoted._
class Test {
  def foo(str: String)(using QuoteContext) = '{
    given QuoteContext = ???
    '{
      @deprecated(str, "") // error
      def bar = ???
    }
  }
}
