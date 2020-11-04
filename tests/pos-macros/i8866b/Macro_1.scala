import scala.quoted._

object Other {
  inline def apply = 5
}

object Macro {

  def impl(using qctx: QuoteContext): Expr[Int] = {
    import qctx.reflect._

    ValDef.let(
      Select.unique(
        '{ Other }.unseal,
        "apply"
      )
    )(identity).seal.cast[Int]

  }

  inline def apply = ${ Macro.impl }

}
