import scala.quoted._

object OtherMacro {

  def impl(using Scope): scope.Expr[Int] =
    '{ 42 }

  inline def apply = ${ OtherMacro.impl }

}

object Macro {

  def impl(using Scope): scope.Expr[Int] = {
    import scope.tasty._

    let(
      Select.unique(
        '{ OtherMacro },
        "apply"
      )
    )(identity).seal.cast[Int]
  }

  inline def apply = ${ Macro.impl }

}
