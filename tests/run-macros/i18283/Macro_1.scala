import scala.quoted.*

object Macro:
  transparent inline def getType[T] =
    ${ getTypeImpl[T] }
  private def getTypeImpl[T: Type](using Quotes): Expr[Any] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val reprShow = tpe.show
    tpe.asType match
      case '[t] =>
        val typeShow = TypeRepr.of[t].show // dealiased type
        Expr((Type.show[T], reprShow, typeShow))
