import scala.quoted._

object Macros {
  transparent inline def defaultOf(inline str: String): Any = ${ defaultOfImpl('str) }
  def defaultOfImpl(using s: Scope)(str: s.Expr[String]): s.Expr[Any] = str.unliftOrError match {
    case "int" => '{1}
    case "string" => '{"a"}
  }
}
