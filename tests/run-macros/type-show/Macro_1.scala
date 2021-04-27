import scala.quoted.*

object TypeToolbox {
  inline def show[A]: String = ${ showImpl[A] }
  private def showImpl[A: Type](using Quotes) : Expr[String] =
    Expr(Type.show[A])
}
