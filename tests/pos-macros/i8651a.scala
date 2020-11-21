import scala.quoted._
def coroutineImpl(using Quotes): Expr[Any] =
  '{
    new {
      def state: Int = 0
      ${identity('state)}
    }
  }
