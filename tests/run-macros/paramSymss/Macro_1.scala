import scala.quoted._

inline def showParamSyms(inline x: Any): String =
  ${ showParamSymsExpr('x) }

def showParamSymsExpr(using s: Scope)(x: s.Expr[Any]): s.Expr[String] =
  val '{ $y: Any } = x // Drop Inlined not to access the symbol
  val sym = y.symbol
  Expr(
    s"""sym: ${sym.show}
       |paramSymss: ${sym.paramSymss.map(_.map(_.show))}
       |""".stripMargin)
