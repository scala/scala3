import scala.quoted._
object Macro {
  transparent def ff(args: Any*): String = ~impl('(args))
  def impl(args: Expr[Seq[Any]]): Expr[String] = '("")
}
