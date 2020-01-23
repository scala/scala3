import scala.quoted._
class Test {
  def foo(str: String) with QuoteContext = '{
    @deprecated(str, "") // error
    def bar = ???
  }
}
