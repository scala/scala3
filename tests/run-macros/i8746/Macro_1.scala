import scala.quoted.*

object Macro {
  inline def mac(): String = ${ macImpl() }
  def macImpl()(using Quotes): Expr[String] =
    '{(x: String) => "anything"} match
      case '{ (in: String) => ($out: tpe2) } => Expr(out.toString)
      case _ => ???

}
