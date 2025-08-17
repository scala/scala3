import scala.quoted.*

object Macro:
  inline def myMacro[T](): String =
    ${ myMacroImpl[T]() }

  def myMacroImpl[T: Type]()(using Quotes): Expr[String] =
    import quotes.reflect.*
    val myTypeRepr = MyTypeRepr(TypeRepr.of[T])
    val `caseName`(name) = myTypeRepr.requiredAnnotationValue[caseName]
    Expr(name)
