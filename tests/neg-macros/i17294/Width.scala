// nopos-error
package crash
import scala.quoted.*

trait Width[T]:
  type Out
object Width:
  transparent inline given [T]: Width[T] = ${ getWidthMacro[T] }
  def getWidthMacro[T](using Quotes, Type[T]): Expr[Width[T]] = '{ new Width[T] {} }
end Width

val x = bits(1)