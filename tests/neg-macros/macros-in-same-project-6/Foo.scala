import scala.quoted._

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(given qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty._
    error("some error", rootPosition)
    throw new NoClassDefFoundError("Bar$")
  }
}
