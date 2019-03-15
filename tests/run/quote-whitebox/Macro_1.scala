import scala.quoted._

object Macros {
  inline def defaultOf(inline str: String) <: Any = ${ defaultOfImpl(str) }
  def defaultOfImpl(str: String): Expr[Any] = str match {
    case "int" => '{1}
    case "string" => '{"a"}
  }
}
