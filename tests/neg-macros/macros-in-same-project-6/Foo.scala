import scala.quoted._

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation with (qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty.{given, _}
    error("some error", rootPosition)
    throw new NoClassDefFoundError("Bar$")
  }
}
