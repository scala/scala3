import scala.quoted._

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._
    Reporting.error("some error", rootPosition)
    throw new NoClassDefFoundError("Bar$")
  }
}
