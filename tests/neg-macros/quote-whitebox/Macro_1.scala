import scala.quoted._

object Macros {
  inline def defaultOf(inline str: String) <: Any = ${ defaultOfImpl('str) }
  def defaultOfImpl(str: Expr[String]) (using QuoteContext): Expr[Any] = str.value match {
    case "int" => '{1}
    case "string" => '{"a"}
  }
}
