import scala.quoted._

object Lib {
  inline def sum(inline args: Int*): Int = ${ impl('args) }
  def impl(using s: Scope)(args: s.Expr[Seq[Int]]): s.Expr[Int] = Expr(args.unliftOrError.sum)
}
