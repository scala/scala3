import scala.quoted.{Expr, Quotes, Type}

object Macros {
  enum Free[A, B](using val typeDom: Type[A], val typeCod: Type[B]):
    case Id[A: Type]() extends Free[A,A]
    case Product[A: Type, B: Type, C: Type](f: Free[A,B], g: Free[A,C])(using quotes: Quotes)
      extends Free[A,(B,C)](using typeCod = Type.of[(B,C)])()

  def test[A: Type]()(using quotes: Quotes): Expr[Unit] =
    val prod = Free.Product[A,A,A](Free.Id[A](), Free.Id[A]())
    '{ () }

  inline def testInline[A](): Unit = ${ test[A]() }
}
