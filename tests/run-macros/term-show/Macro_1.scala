import scala.quoted.*

object TypeToolbox {
  inline def show(inline v: Any): String = ${ showImpl('v) }
  private def showImpl(using Quotes)(v: Expr[Any]): Expr[String] =
    import quotes.reflect.*
    Expr(v.show)
}
