import scala.quoted._
object Test {
  def impl(receiver: Expr[StringContext]) given (qctx: scala.quoted.QuoteContext) = {
    import qctx.tasty.Repeated
    receiver match {
      case '{ StringContext(${Repeated(parts)}: _*) } => // error
    }
  }
}
