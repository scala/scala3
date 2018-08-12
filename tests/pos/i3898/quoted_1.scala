import scala.quoted._
object Macro {
  rewrite def ff(args: Any*): String = ~impl('(args))
  def impl(args: Expr[Seq[Any]]): Expr[String] = '("")
}
