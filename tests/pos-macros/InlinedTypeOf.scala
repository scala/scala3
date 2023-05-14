import scala.quoted._

class Sm[T](t: T)

object Foo {

  inline def foo[T] = { compiletime.summonInline[Type[T]]; ??? }

  def toexpr[T: Type](using Quotes) = foo[Sm[T]]

}
