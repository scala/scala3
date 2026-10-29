//> using options -Yno-suspended-units

import scala.quoted.*
trait Width[T]
object Width:
  transparent inline given [T]: Width[T] = ${ widthImpl[T] }
def widthImpl[T: Type](using Quotes): Expr[Width[T]] = '{ new Width[T] {} }
