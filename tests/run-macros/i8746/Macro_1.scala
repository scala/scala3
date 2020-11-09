import scala.quoted._

object Macro {
  inline def mac(): String = ${ macImpl() }
  def macImpl()(using qctx: QuoteContext): Expr[String] =
    '{(x: String) => "anything"} match
      case '{ (in: String) => ($out: tpe2) } => Expr(out.toString)
      case _ => ???

}
