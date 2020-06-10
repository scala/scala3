import scala.compiletime.erasedValue

type Foo = Foo.type
object Foo

inline def fooErased[T] = inline erasedValue[T] match { case _ => }
val f = fooErased[Foo]
