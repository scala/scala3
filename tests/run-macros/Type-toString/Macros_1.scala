import scala.quoted.*

inline def showToString[T]: String = ${ macroImpl[T] }

private def macroImpl[T: Type](using Quotes): Expr[String] =
  Expr(Type.of[T].toString)
