import annotation.*

class Leak()(using @constructorOnly l: boundary.Label[String]) {
  Seq("a", "b").foreach(_ => boundary.break("stop"))
}

object boundary {
  final class Break[T] private[boundary](val label: Label[T], val value: T)
    extends RuntimeException(
          /*message*/ null, /*cause*/ null, /*enableSuppression=*/ false, /*writableStackTrace*/ false)
  final class Label[-T]
  def break[T](value: T)(using label: Label[T]): Nothing = throw new Break(label, value)
}
