import scala.quoted.*

inline def structureOf[T]: String =
  ${ structureOfImpl[T] }

private def structureOfImpl[T](using Quotes, Type[T]): Expr[String] =
  import quotes.reflect.*
  val str = Printer.TypeReprStructure.show(TypeRepr.of[T])
  Expr(str)
