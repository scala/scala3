import scala.quoted._

inline def showParamSyms(inline x: Any): String =
  ${ showParamSymsExpr('x) }

def showParamSymsExpr(using QuoteContext)(x: Expr[Any]): Expr[String] =
  val '{ $y: Any } = x // Drop Inlined not to access the symbol
  val sym = y.asReflectTree.symbol
  Expr(
    s"""sym: ${sym.show}
       |paramSymss: ${sym.paramSymss.map(_.map(_.show))}
       |""".stripMargin)
