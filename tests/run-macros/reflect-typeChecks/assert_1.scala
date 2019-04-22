import scala.quoted._
import scala.tasty._

object scalatest {

  inline def assertCompile(inline code: String): Unit = ${ assertImpl(code, true) }
  inline def assertNotCompile(inline code: String): Unit = ${ assertImpl(code, false) }

  def assertImpl(code: String, expect: Boolean)(implicit refl: Reflection): Expr[Unit] = {
    import refl._

    val actual = typing.typeChecks(code)

    '{ assert(${expect.toExpr} == ${actual.toExpr}) }
  }
}
