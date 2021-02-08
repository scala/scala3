//macros
import scala.quoted._

object Macros {
  //a specialization of the `findOwner` function from `sourcecode` for our purposes
  private def firstNonSyntheticOwner(using Quotes)(s: quotes.reflect.Symbol): quotes.reflect.Symbol = {
    import quotes.reflect._
    if (s.flags.is(Flags.Synthetic)) firstNonSyntheticOwner(s.owner)
    else s
  }

  def genOwnerImpl()(using Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(firstNonSyntheticOwner(Symbol.spliceOwner).name)
  }
}

object Foo {
  inline def genOwner: String = ${ Macros.genOwnerImpl() }
}
