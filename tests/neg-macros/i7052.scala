import scala.quoted._
class Test {
  def foo(using s: Scope)(str: String) = '{
    @deprecated(str, "") // error
    def bar = ???
  }
}
