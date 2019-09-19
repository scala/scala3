import scala.quoted.{_, given}
class Test {
  def foo(str: String)(given QuoteContext) = '{
    given QuoteContext = ???
    '{
      @deprecated(str, "") // error
      def bar = ???
    }
  }
}
