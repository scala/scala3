import scala.quoted.*
class Test {
  def foo(str: String)(using Quotes) = '{
    val qctx: Quotes = ???
    given qctx.type = qctx
    '{
      @deprecated(str, "") // error
      def bar = ???
    }
  }
}
