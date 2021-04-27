import scala.quoted.*

object Other {
  inline def apply = 5
}

object Macro {

  def impl(using Quotes): Expr[Int] = {
    import quotes.reflect.*

    ValDef.let(
      Symbol.spliceOwner,
      Select.unique(
        '{ Other }.asTerm,
        "apply"
      )
    )(identity).asExprOf[Int]

  }

  inline def apply = ${ Macro.impl }

}
