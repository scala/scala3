// macro.scala
package prelude
import scala.quoted.*

object Macros {
  def validateInlineImpl[A: Type](assertionExpr: Expr[Assertion[A]], a: Expr[A])(using Quotes): Expr[Unit] =
    '{ () }
}
