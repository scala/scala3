import scala.quoted._

object Macros {

  inline def lift[A]: String = ${ matchesExpr[A] }

  private def matchesExpr[A](using tp: Type[A])(using QuoteContext): Expr[String] = {
    def lift[T](using tp: Type[T]): String = tp match {
      case '[Int] => "%Int%"
      case '[List[$T]] => s"%List[${lift[T]}]%"
      case '[Option[$T]] => s"%Option[${lift[T]}]%"
      case '[Function1[$T, $U]] => s"%${lift[T]} => ${lift[U]}%"
      case _ => Type.show[T]
    }
    Expr(lift[A])
  }

}
