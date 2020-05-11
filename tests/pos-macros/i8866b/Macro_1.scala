import scala.quoted._

object Other {
  inline def apply = 5
}

object Macro {

  def impl(using Scope): scope.Expr[Int] = {
    import scope.tasty._

    let(
      Select.unique(
        '{ Other },
        "apply"
      )
    )(identity).seal.cast[Int]

  }

  inline def apply = ${ Macro.impl }

}
