import scala.quoted._
import scala.quoted.autolift._
import scala.tasty._

object Macros {

  inline def test(): String = ${ testImpl }

  private def testImpl(implicit reflect: Reflection): Expr[String] = {
    import reflect._
    typeOf[Function1[_, _]].classSymbol.get.methods.map(_.name).sorted.mkString("\n")
  }

}
