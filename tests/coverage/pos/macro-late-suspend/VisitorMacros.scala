package example

import scala.quoted.*

private def mkVisitorTypeImpl[T: Type](using q: Quotes): Expr[VisitorType[T]] =
  '{new VisitorType[T]{}}

transparent inline def mkVisitorType[T]: VisitorType[T] = ${ mkVisitorTypeImpl[T] }

trait VisitorType[T] {
  type Out[A]
}
