package prelude
import scala.quoted.*

object Macros {
  def validateInlineImpl[A: Type](assertionExpr: Expr[Assertion[A]], a: Expr[A])(using
      Quotes
  ): Expr[Unit] = {
    import quotes.reflect.*
    val crashRoot = assertionExpr.value
    '{ () }

  }
  given [A](using Type[A]): FromExpr[Assertion[A]] with {
    def unapply(assertion: Expr[Assertion[A]])(using Quotes): Option[Assertion[A]] = {
      import quotes.reflect.*

      assertion match {
        case '{ Assertion.greaterThanOrEqualTo[A](${ LiteralUnlift(value) })($_) } =>
          Some(Assertion.greaterThanOrEqualTo(value)(orderingForValue(value)))
        case _ => None
      }
    }
  }

  object LiteralUnlift {
    def unapply[A: Type](expr: Expr[A])(using Quotes): Option[A] = expr match {
      case '{ ${ Expr(int) }: Int }       => Some(int)
      case '{ Int.MaxValue }              => Some(Int.MaxValue.asInstanceOf[A])
      case '{ Int.MinValue }              => Some(Int.MinValue.asInstanceOf[A])
      case '{ ${ Expr(string) }: String } => Some(string)
      case '{ ${ Expr(double) }: Double } => Some(double)
      case '{ ${ Expr(float) }: Float }   => Some(float)
      case '{ ${ Expr(long) }: Long }     => Some(long)
      case '{ ${ Expr(short) }: Short }   => Some(short)
      case '{ ${ Expr(byte) }: Byte }     => Some(byte)
      case '{ ${ Expr(char) }: Char }     => Some(char)
      case _                              => None
    }
  }

  private def orderingForValue(any: Any)(using Quotes): Ordering[Any] = null.asInstanceOf[Ordering[Any]]
}
