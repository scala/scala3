import scala.quoted._
import scala.quoted.autolift.{given _}

object Macros {

  inline def test(): String = ${ testImpl }

  private def testImpl with (qctx: QuoteContext) : Expr[String] = {
    import qctx.tasty.{_, given _}
    val classSym = typeOf[Function1[_, _]].classSymbol.get
    classSym.classMethod("apply")
    classSym.classMethods
    classSym.method("apply")
    classSym.methods.map(_.name).sorted.mkString("\n")
  }

}
