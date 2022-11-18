import scala.quoted._

class Sm[T](t: T)

object Foo {

  transparent inline def foo[T] = { compiletime.summonInline[Type[T]]; ??? }

  def toexpr[T: Type](using Quotes) = foo[Sm[T]]

}
