import scala.quoted._
object Macro {
  inline def ff(args: Any*): String = ${impl('args)}
  def impl(using s: Scope)(args: s.Expr[Seq[Any]]): s.Expr[String] = '{""}
}
