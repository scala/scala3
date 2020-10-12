import scala.quoted._
object Test {
  def impl(receiver: Expr[StringContext])(using qctx: scala.quoted.QuoteContext) = {
    import qctx.reflect.Repeated
    receiver match {
      case '{ StringContext(${Repeated(parts)}: _*) } => // now OK
    }
  }
}
