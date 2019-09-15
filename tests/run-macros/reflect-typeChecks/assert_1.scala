import scala.quoted._

object scalatest {

  inline def assertCompile(inline code: String): Unit = ${ assertImpl(code, compiletime.testing.typeChecks(code), true) }
  inline def assertNotCompile(inline code: String): Unit = ${ assertImpl(code, compiletime.testing.typeChecks(code), false) }

  def assertImpl(code: String, actual: Boolean, expect: Boolean)(given qctx: QuoteContext): Expr[Unit] = {
    '{ assert(${expect.toExpr} == ${actual.toExpr}) }
  }
}
