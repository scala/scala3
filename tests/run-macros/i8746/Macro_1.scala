import scala.quoted._

object Macro {
  inline def mac(): String = ${ macImpl() }
  def macImpl()(using Quotes): Expr[String] =
    '{(x: String) => "anything"} match
      case '{ (in: String) => ($out: tpe2) } => Value(out.toString)
      case _ => ???

}
