import scala.quoted.*

object OtherMacro {

  def impl(using Quotes): Expr[Int] =
    '{ 42 }

  inline def apply = ${ OtherMacro.impl }

}

object Macro {

  def impl(using Quotes): Expr[Int] = {
    import quotes.reflect.*

    ValDef.let(
      Symbol.spliceOwner,
      Select.unique(
        '{ OtherMacro }.asTerm,
        "apply"
      )
    )(identity).asExprOf[Int]
  }

  inline def apply = ${ Macro.impl }

}
