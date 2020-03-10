import scala.quoted._
class Test {
  def foo(str: String)(using QuoteContext) = '{
    val qctx: QuoteContext = ???
    given qctx.type = qctx
    '{
      @deprecated(str, "") // error
      def bar = ???
    }
  }
}
