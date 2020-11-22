import scala.quoted._

object Macros {

  inline def test(): String = ${ testImpl }

  private def testImpl(using Quotes) : Expr[String] = {
    import quotes.reflect._
    val classSym = TypeRepr.of[Function1].classSymbol.get
    classSym.classMethod("apply")
    classSym.classMethods
    classSym.method("apply")
    Expr(classSym.methods.map(_.name).sorted.mkString("\n"))
  }

}
