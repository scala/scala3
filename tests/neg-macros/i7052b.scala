import scala.quoted._
class Test {
  def foo(using s: Scope)(str: String) = '{
    val s: Scope = ???
    given s.type = s
    '{
      @deprecated(str, "") // error
      def bar = ???
    }
  }
}
