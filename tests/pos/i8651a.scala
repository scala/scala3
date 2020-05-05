import scala.quoted._
def coroutineImpl(using QuoteContext): Expr[Any] =
  '{
    new {
      def state: Int = 0
      ${identity('state)}
    }
  }
