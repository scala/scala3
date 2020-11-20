import scala.quoted._

object OtherMacro {

  def impl(using qctx: QuoteContext): Expr[Int] =
    '{ 42 }

  inline def apply = ${ OtherMacro.impl }

}

object Macro {

  def impl(using qctx: QuoteContext): Expr[Int] = {
    import qctx.reflect._

    ValDef.let(
      Symbol.currentOwner,
      Select.unique(
        Term.of('{ OtherMacro }),
        "apply"
      )
    )(identity).asExprOf[Int]
  }

  inline def apply = ${ Macro.impl }

}
