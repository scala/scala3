import scala.quoted.*

object InvokeConstructor {
  inline def apply[A] = ${ constructorMacro[A] }

  def constructorMacro[A: Type](using Quotes) = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[A]
    New(Inferred(tpe)).select(tpe.typeSymbol.primaryConstructor).appliedToArgs(Nil).asExprOf[A]
  }
}
