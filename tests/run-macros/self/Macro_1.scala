import scala.quoted.*

trait A {
  type X
}
trait B { self: A =>
  def foo(x: Int): X
  def foo(x: String): X
}

object Obj {
  def foo: Int = 1
}

object Macros {

  inline def test(): String = ${ testImpl }

  private def testImpl(using Quotes) : Expr[String] = {
    import quotes.reflect.*
    val bTpe = TypeRepr.of[B]
    val bSym = bTpe.classSymbol.get
    val bMethSyms = bSym.methodMember("foo") // Used to throw a MissingType exception
    val bMethTpes = bMethSyms.map(bTpe.memberType)

    // Make sure we didn't break member lookup on terms
    val objTpe = TypeRepr.of[Obj.type]
    val objSym = objTpe.termSymbol
    val objMethSyms = objSym.methodMember("foo")
    val objMethTpes = objMethSyms.map(objTpe.memberType)

    Expr((objMethTpes ++ bMethTpes).map(_.toString).sorted.mkString("\n"))
  }

}
