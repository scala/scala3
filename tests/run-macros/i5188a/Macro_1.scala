import scala.quoted.*

object Lib {
  inline def sum(inline args: Int*): Int = ${ impl('args) }
  def impl(args: Expr[Seq[Int]]) (using Quotes): Expr[Int] = Expr(args.valueOrError.sum)
}
