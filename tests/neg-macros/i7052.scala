import scala.quoted.*
class Test {
  def foo(str: String)(using Quotes) = '{
    @deprecated(str, "") // error
    def bar = ???
  }
}
