import scala.quoted.{_, given}
object Macro {
  inline def ff(args: Any*): String = ${impl('args)}
  def impl(args: Expr[Seq[Any]])(given QuoteContext): Expr[String] = '{""}
}
