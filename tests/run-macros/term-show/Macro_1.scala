import scala.quoted.*

object TypeToolbox {
  inline def show(inline v: Any): String = ${ showImpl('v) }
  private def showImpl(using Quotes)(v: Expr[Any]): Expr[String] =
    import quotes.reflect.*
    Expr(v.show)

  inline def showTree(inline className: String): String = ${ showTreeImpl('className) }
  private def showTreeImpl(className: Expr[String])(using Quotes) : Expr[String] =
    import quotes.reflect.*
    val name = className.valueOrAbort
    val res = Symbol.requiredClass(name).tree.show
    Expr(res)
}
