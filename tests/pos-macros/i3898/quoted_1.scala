import scala.quoted._
object Macro {
  inline def ff(args: Any*): String = ${impl('args)}
  def impl(args: Expr[Seq[Any]])(using Quotes): Expr[String] = '{""}
}
