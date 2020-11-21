import scala.quoted._
class Test {
  def foo(str: String)(using Quotes) = '{
    @deprecated(str, "") // error
    def bar = ???
  }
}
