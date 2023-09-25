import scala.quoted.*

class Bar[T]
object Bar:
  transparent inline def bar[T](a: Foo, b: a.Out): Bar[T] = ${ getBarMacro[T] }
  def getBarMacro[T](using Quotes, Type[T]): Expr[Bar[T]] = '{ new Bar[T] }