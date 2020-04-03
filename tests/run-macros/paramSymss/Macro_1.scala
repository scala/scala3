import scala.quoted._

inline def showParamSyms(inline x: Any): String =
  ${ showParamSymsExpr('x) }

def showParamSymsExpr(using QuoteContext)(x: Expr[Any]): Expr[String] =
  val '{ $y: Any } = x // Drop Inlined not to access the symbol
  val sym = y.unseal.symbol
  val (tparams, vparamss) = sym.paramSymss
  Expr(
    s"""sym: ${sym.show}
       |tparams: ${tparams.map(_.show)}
       |vparamss: ${vparamss.map(_.map(_.show))}
       |""".stripMargin)
