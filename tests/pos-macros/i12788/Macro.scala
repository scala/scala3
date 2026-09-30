import scala.quoted.*

class InlinedInt[T <: Int]

object DFType:
  trait Width[T]:
    type Out <: Int
  transparent inline given [T]: Width[T] = ${ getWidthMacro[T] }
  def getWidthMacro[T](using Quotes, Type[T]): Expr[Width[T]] =
    '{
      new Width[T] {
        type Out = 1
      }
    }

  trait DFBits[W <: Int]
  object DFBits extends DFBitsCompanion

trait DFToken[T]
extension [T](token: DFToken[T])(using w: DFType.Width[T])
  def width: InlinedInt[w.Out] = ???