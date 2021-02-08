import scala.quoted.*

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*
    report.error("some error", Position.ofMacroExpansion)
    throw new NoClassDefFoundError("Bar$")
  }
}
