import scala.quoted._

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty.{given _, _}
    error("some error", rootPosition)
    throw new NoClassDefFoundError("Bar$")
  }
}
