import scala.quoted._
class Test {
  def foo(str: String) given QuoteContext = '{
    delegate for QuoteContext = ???
    '{
      @deprecated(str, "") // error
      def bar = ???
    }
  }
}
