import scala.quoted.*

trait Width[T]:
  type Out <: Int
object Width:
  transparent inline given [T] => Width[T] = ${ getWidthMacro[T] }
  def getWidthMacro[T](using Quotes, Type[T]): Expr[Width[T]] =
    '{
      new Width[T] {
        type Out = Int
      }
    }
