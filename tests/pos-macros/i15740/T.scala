package f

import scala.quoted._

class T[X]

object T {
  inline given derivedT[X]: T[X] = ${ derivedTImpl[X] }

  def derivedTImpl[X](using t: Type[X])(using ctx: Quotes): Expr[T[X]] = {
    '{ new T }
  }
}
