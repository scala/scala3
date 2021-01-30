import scala.quoted.*

object scalatest {

  inline def assertCompile(inline code: String): Unit = ${ assertImpl('code, '{compiletime.testing.typeChecks(code)}, true) }
  inline def assertNotCompile(inline code: String): Unit = ${ assertImpl('code, '{compiletime.testing.typeChecks(code)}, false) }

  def assertImpl(code: Expr[String], actual: Expr[Boolean], expect: Boolean)(using Quotes) : Expr[Unit] = {
    '{ assert(${Expr(expect)} == $actual) }
  }
}
