import scala.quoted.*

object PullAst:
  def applyImpl(quoted: Expr[MyQuoted])(using qctx: Quotes): Expr[String] =
    '{ $quoted.ast.toString }
  inline def apply(inline quoted: MyQuoted): String =
    ${ applyImpl('quoted) }
