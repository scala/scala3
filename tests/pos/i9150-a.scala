import scala.compiletime.erasedValue

inline def fooErased[T] = inline erasedValue[T] match { case _ => }
val f = fooErased[EmptyTuple]
