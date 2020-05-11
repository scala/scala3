import scala.quoted._

object scalatest {

  inline def assertCompile(inline code: String): Unit = ${ assertImpl('code, '{compiletime.testing.typeChecks(code)}, true) }
  inline def assertNotCompile(inline code: String): Unit = ${ assertImpl('code, '{compiletime.testing.typeChecks(code)}, false) }

  def assertImpl(using s: Scope)(code: s.Expr[String], actual: s.Expr[Boolean], expect: Boolean): s.Expr[Unit] = {
    '{ assert(${Expr(expect)} == $actual) }
  }
}
