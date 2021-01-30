import scala.quoted.*

object Macros {
  transparent inline def defaultOf(inline str: String): Any = ${ defaultOfImpl('str) }
  def defaultOfImpl(str: Expr[String]) (using Quotes): Expr[Any] = str.valueOrError match {
    case "int" => '{1}
    case "string" => '{"a"}
  }
}
