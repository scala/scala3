import scala.quoted._

inline def showParamSyms(inline x: Any): String =
  ${ showParamSymsExpr('x) }

def showParamSymsExpr(using Quotes)(x: Expr[Any]): Expr[String] =
  import qctx.reflect._
  val '{ $y: Any } = x // Drop Inlined not to access the symbol
  val sym = Term.of(y).symbol
  Expr(
    s"""sym: ${sym.show}
       |paramSymss: ${sym.paramSymss.map(_.map(_.show))}
       |""".stripMargin)
