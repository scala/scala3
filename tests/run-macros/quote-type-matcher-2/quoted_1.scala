import scala.quoted.*

object Macros {

  inline def lift[A]: String = ${ matchesExpr[A] }

  private def matchesExpr[A](using tp: Type[A])(using Quotes): Expr[String] = {
    def lift[T](using tp: Type[T]): String = tp match {
      case '[Int] => "%Int%"
      case '[List[t]] => s"%List[${lift[t]}]%"
      case '[Option[t]] => s"%Option[${lift[t]}]%"
      case '[Function1[t, u]] => s"%${lift[t]} => ${lift[u]}%"
      case _ => Type.show[T]
    }
    Expr(lift[A])
  }

}
