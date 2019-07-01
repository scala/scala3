import scala.quoted._
import scala.tasty._

object scalatest {

  inline def assertCompile(inline code: String): Unit = ${ assertImpl(code, true) }
  inline def assertNotCompile(inline code: String): Unit = ${ assertImpl(code, false) }

  def assertImpl(code: String, expect: Boolean) given (qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty._

    val actual = typing.typeChecks(code)

    '{ assert(${expect.toExpr} == ${actual.toExpr}) }
  }
}
