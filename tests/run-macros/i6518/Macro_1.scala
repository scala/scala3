import scala.quoted._

object Macros {

  inline def test(): String = ${ testImpl }

  private def testImpl(using s: Scope): s.Expr[String] = {
    import s.tasty._
    val classSym = Type.of[Function1[_, _]].classSymbol.get
    classSym.classMethod("apply")
    classSym.classMethods
    classSym.method("apply")
    Expr(classSym.methods.map(_.name).sorted.mkString("\n"))
  }

}
